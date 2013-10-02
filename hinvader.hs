import Graphics.UI.GLUT hiding (Bitmap)
import qualified Graphics.Rendering.OpenGL.GL as GL
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import Data.IORef
import Codec.Picture.Repa
import Data.Array.Repa as R hiding (reshape , map )
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Foreign.ForeignPtr
import Data.Word()
import Control.Applicative
import Control.Monad
import Unsafe.Coerce

data Fighter = Fighter { xfighter::GLfloat , yfighter::GLfloat }
data Bullet = Bullet { xbullet::GLfloat , ybullet::GLfloat ,lbullet::Int  }
data Enemy = Enemy { xenemy::GLfloat , yenemy::GLfloat , lenemy::Int }
data Flame = Flame { xflame::GLfloat , yflame::GLfloat , lflame::Int }
data State = Start | Main deriving (Eq)
data Game = Game { fighter::Fighter,bullets::[Bullet],enemies::[Enemy],flames::[Flame],state::State,ftex::TextureObject,btex::TextureObject,etex::TextureObject,fltex::TextureObject,sttex::TextureObject }

cz :: GLfloat
cz = 50 -- character size

timerInterval :: Timeout
timerInterval = 40

inite h = map (\i->(Enemy i h 1)) [100,170..500]

main :: IO ()
main = do 
    
    --GLUTの初期化
    initialDisplayMode $= [RGBAMode, DoubleBuffered]
    initialWindowSize $= Size 640 480
    initialize "" []

    --ウィンドウを作る
    createWindow "invader"

    --テクスチャを読み込む
    texbullet <- loadTextureFromFile "./bullet.jpg"
    texfighter <- loadTextureFromFile "./fighter.jpg"
    texenemy <- loadTextureFromFile "./enemy.jpg"
    texflame <- loadTextureFromFile "./flame.jpg"
    texstart <- loadTextureFromFile "./start.jpg"
    texture Texture2D $= Enabled
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    
    game <- newIORef (Game (Fighter 50 50) [(Bullet (1000) 50 0),(Bullet (1000) 50 0),(Bullet (1000) 50 0)] (concat [inite 450,inite 380,inite 310]) [] Start texfighter texbullet texenemy texflame texstart)
    

    displayCallback $= display game
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardProc game)
    addTimerCallback timerInterval $ timerProc (display game)
    mainLoop

processBullet :: [Enemy] -> [Bullet] -> [Bullet]
--processBullet es bs = ((map f1) . (map f2) . (map f3)) bs
processBullet es bs = map (f1 . f2 . f3) bs
    where f1 b = b{ybullet=ybullet b+4}
          f2 b = if (ybullet b)>505 then b{lbullet=0} else b
          f3 b = if any (\e->g1 (lenemy e)>0&&g1 (xbullet b-xenemy e)<cz/2&&g1 (ybullet b-yenemy e)<cz/2) es
                    then b{lbullet=0}
                    else b
          g1 f = if f>0 then f else -f

processEnemy :: [Bullet] -> [Enemy] -> [Enemy]
--processEnemy bs es = ((map f4).(map f1).(map f2)) es
processEnemy bs es = map (f4 . f1 . f2) es
    where f1 e = if lenemy e`div`20`mod`2==0 then e{xenemy=xenemy e-2} else e{xenemy=xenemy e+2}
          f2 e = if any (\b->(lbullet b)>0&&g1 (xbullet b-xenemy e)<cz/2&&g1 (ybullet b-yenemy e)<cz/2) bs
                    then e{lenemy=0}
                    else e     
          g1 f = if f>0 then f else -f
          f4 e = if lenemy e>0 then e{yenemy=yenemy e-0.2,lenemy=lenemy e+1} else e

processFlame :: [Bullet] -> [Enemy] -> [Flame] -> [Flame]
processFlame bs es fs = ((Prelude.++ f4) . (filter f2) . (map f1)) fs
    where f1 f = f{lflame=lflame f-1}
          f2 f = (lflame f) > 0
          f3 (b,e) = if (lbullet b)>0&&(lenemy e)>0&&
                        g1 (xbullet b-xenemy e)<cz/2&&g1 (ybullet b-yenemy e)<cz/2
                        then [Flame {xflame=xenemy e,yflame=yenemy e,lflame=10}]
                        else []
          f4 = concat (map f3 g2)
          g1 f = if f>0 then f else -f
          g2 = concat (map (\b-> (map (\e->(b,e)) es)) bs)
          
processGame :: Game -> Game
processGame g = if state g == Main 
                   then g{bullets=processBullet (enemies g) (bullets g),
                              enemies=processEnemy (bullets g) (enemies g),
                              flames=processFlame (bullets g) (enemies g) (flames g)}
                   else g     

display :: IORef Game -> IO ()
display game = do
    g <-readIORef game
    modifyIORef game processGame
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clear [ColorBuffer]
    loadIdentity
    
    renderGame g
    swapBuffers

renderGame :: Game -> IO()
renderGame g = if state g == Main 
    then do
         mapM_ (renderBullet (btex g)) (bullets g)
         mapM_ (renderEnemy (etex g)) (enemies g)
         mapM_ (renderFlame (fltex g)) (flames g)
         render (xfighter (fighter g)) (yfighter (fighter g)) (ftex g)
    else do
         currentColor $= Color4 1 1 1 1 
         textureBinding Texture2D $= Just (sttex g)
         preservingMatrix $ do
           -- translate (Vector3 0 0 (0::Double))
             renderPrimitive Quads $ do
                 texCoord2f (TexCoord2 0 0)
                 vertex3f (Vertex3 0 0 0.0)
                 texCoord2f (TexCoord2 0 1)
                 vertex3f (Vertex3 0 480 0.0)
                 texCoord2f (TexCoord2 1 1)
                 vertex3f (Vertex3 640 480 0.0)
                 texCoord2f (TexCoord2 1 0)
                 vertex3f (Vertex3 640 0 0.0)        

renderBullet :: TextureObject -> Bullet -> IO ()
renderBullet texbullet b = if lbullet b > 0 then render (xbullet b) (ybullet b) texbullet
                                            else return ()     

renderEnemy :: TextureObject -> Enemy -> IO ()          
renderEnemy texenemy e = if lenemy e > 0 then render (xenemy e) (yenemy e) texenemy
                                         else return ()     

renderFlame :: TextureObject -> Flame -> IO()
renderFlame texflame f = if lflame f > 0 then render (xflame f) (yflame f) texflame
                                         else return ()     

render :: GLfloat -> GLfloat -> TextureObject -> IO ()
render x y tex = do
    currentColor $= Color4 1 1 1 1 
    textureBinding Texture2D $= Just tex
    preservingMatrix $ do
        -- translate (Vector3 0 0 (0::Double))
        renderPrimitive Quads $ do
            texCoord2f (TexCoord2 0 0)
            vertex3f (Vertex3 (x-cz/2) (y-cz/2) 0.0)
            texCoord2f (TexCoord2 0 1)
            vertex3f (Vertex3 (x-cz/2) (y+cz/2) 0.0)
            texCoord2f (TexCoord2 1 1)
            vertex3f (Vertex3 (x+cz/2) (y+cz/2) 0.0)
            texCoord2f (TexCoord2 1 0)
            vertex3f (Vertex3 (x+cz/2) (y-cz/2) 0.0)

texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
vertex3f = vertex :: Vertex3 GLfloat -> IO ()
          

--タイマが呼ばれるたびにactを繰り返す
timerProc act = do
    act
    addTimerCallback timerInterval $ timerProc act

--ウィンドウのサイズが変更された時の処理
reshape :: Size -> IO()
reshape (Size w h)=do
    viewport $= (Position 0 0, (Size w h)) --ウィンドウ全体を使う
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 640.0 0.0 480.0 (-1000.0) 1000.0
    matrixMode $= Modelview 0 

keyboardProc :: IORef Game -> Key -> t -> t1 -> t2 -> IO ()
keyboardProc game ch st _ _
    | ch == SpecialKey KeyLeft  = modifyIORef game (\g->g{fighter=changeX (-2) (fighter g)})
    | ch == SpecialKey KeyRight = modifyIORef game (\g->g{fighter=changeX 2 (fighter g)})
    | ch == Char ' ' = space (readIORef game)
    | otherwise            = return ()  
    where changeX dx f = if 0+cz<=xfighter f+dx&&xfighter f+dx<=640-cz 
                            then  f { xfighter = xfighter f + dx }
                            else f
          shot = modifyIORef game (\g->g{bullets=shotProcess (fighter g) (bullets g)})
          space g' = do
              g <- g'
              if state g == Main 
                  then shot 
                  else modifyIORef game (\g->g{state=Main})
    
          
shotProcess :: Fighter -> [Bullet] -> [Bullet]
shotProcess f bs = if (all (\b->ybullet b-50>yfighter f) bs) && (any (\b->lbullet b==0) bs)
                       then concat [[(head (filter (\b->lbullet b==0) bs)){xbullet=xfighter f,ybullet=yfighter f,lbullet=1}],
                                    (tail (filter (\b->lbullet b==0) bs)),
                                    (filter (\b->lbullet b/=0) bs)]
                       else bs


-- テクスチャをファイルから読み込む
-- 何をやっているのか未だによくわからない
-- FreeGameのサンプルをちょっといじったもの
loadTextureFromFile :: FilePath -> IO GL.TextureObject
loadTextureFromFile path = do
    content <- delay <$> (flipVertically.imgData) <$> either error id <$> (readImageRGBA path)
    let (Z :. width :. height :. _) = R.extent content
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D GL.$= Just tex
    GL.textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    fptr <- liftM RF.toForeignPtr $ R.computeP $ content
    withForeignPtr fptr
        $ GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (gsizei width) (gsizei height)) 0
        . GL.PixelData GL.RGBA GL.UnsignedInt8888
    return tex

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x

