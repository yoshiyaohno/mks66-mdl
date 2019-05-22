module Interpret where

import Parser
import Line
import Screen
import DrawMats
import Lighting
import qualified Solids as S
import qualified Transform as S

import System.IO
import System.Process
import System.Directory
import System.Environment
import Control.Monad.State
import qualified Data.List as L

cmd :: (MonadState DrawMats m, MonadIO m) => Command -> m ()
cmd c = case c of
    CmdPush                 -> push
    CmdPop                  -> pop
    CmdBox      a b c d     -> box      a b c d 
    CmdSphere   a b c d     -> sphere   a b c d 
    CmdTorus    a b c d e   -> torus    a b c d e
    CmdLine     a b c d e   -> line     a b c d e
    CmdMove     a b         -> move     a b
    CmdScale    a b         -> scale    a b
    CmdRotate   a b c       -> rote     a b c
    _                       -> return ()

rote :: (MonadState DrawMats m) => Axis -> Db -> MS -> m ()
rote ax theta _ = modify . modTransform $ (mappend $ roti ax theta)
    where roti ax theta
            | axis == AxisX = T.rotX (-theta)
            | axis == AxisY = T.rotY (-theta)
            | axis == AxisZ = T.rotZ (-theta)

scale :: (MonadState DrawMats m) => Vec3 -> MS -> m ()
scale (x,y,z) _ = modify . modTransform $ (mappend $ T.scale x y z)

move :: (MonadState DrawMats m) => Vec3 -> MS -> m ()
move (x,y,z) _ = modify . modTransform $ (mappend $ T.trans x y z)

line :: (MonadState DrawMats m) => MS -> Vec3 -> MS -> Vec3 -> MS -> m ()
line args = do
    dm <- get
    let ln = Line (T.pmult (getTransform dm) (Vect x0 y0 z0 1))
                  (T.pmult (getTransform dm) (Vect x1 y1 z1 1))
    modify . modScreen $ drawLine red (fmap round ln)

box :: (MonadState DrawMats m) => MS -> Vec3 -> Vec3 -> MS -> m ()
box _ (cx,cy,cz) (w,h,d) _ = do
    dm <- get
    let tris = S.box cx cy cz w h d
    drawTriangles $ trTris dm tris

sphere :: (MonadState DrawMats m) => MS -> Vec3 -> Db -> MS -> m ()
sphere _ (cx,cy,cz) r _ = do
    dm <- get
    let tris = S.sphere cx cy cz r
    drawTriangles $ trTris dm tris
    
torus :: (MonadState DrawMats m) => MS -> Vec3 -> Db -> Db -> MS -> m ()
torus _ (cx,cy,cz) r0 r1 _ = do
    dm <- get
    let tris = S.torus cx cy cz r0 r1
    drawTriangles $ trTris dm tris

push :: (MonadState DrawMats m) => m ()
push = modify pushTransform

pop :: (MonadState DrawMats m) => m ()
pop = modify popTransform

