{-# LANGUAGE FlexibleContexts #-}
module Lighting where

import Line
import Screen
import Solids
import DrawMats

import Data.Array.Unboxed
import Control.Monad.State
import qualified Data.Set as S

drawTriangle :: (MonadState DrawMats m) => Triangle Double -> m ()
drawTriangle tr = do
    dm <- get
    let pxs = scanTriangle tr
        (toDraw, newZB) = runState (plotPxs pxs) (getZBuf dm)
        c = colorTriangle tr
    put $ dm {getZBuf = newZB}
    modify.modScreen $ draw [((pgetX p, pgetY p), c) | p <- S.toList toDraw]

drawTriangles :: (MonadState DrawMats m) => [Triangle Double] -> m ()
drawTriangles = mapM_ drawTriangle . bfCull

colorTriangle :: Triangle Double -> Color
--hardcoded light moments
colorTriangle tr =
    let norm = normalize $ normal tr
        view = Vect 0 0 1 0
        hhhh = normalize $ Vect 0 1 1 0
        lght = Vect 0 1 0 0
        ka   = 0.1; kd = 0.3; ks = 0.9
        ia   = 255; id = 255; is = 255
        alph = 14
        lDn  = max 0 $ lght`dot`norm
        hDn  = max 0 $ hhhh`dot`norm
        intensity = min 255 $ round
            (ka*ia + kd*lDn*id + is*ks*(hDn**alph))
    in
        color intensity intensity intensity

plotPxs :: (MonadState ZBuf m) => S.Set Pixel -> m (S.Set Pixel)
plotPxs pxs = do
    zb <- get
    let ok = [p | (Pixel p) <- S.toList pxs, inRange (bounds zb) (fst p),
                                zb!(fst p) > snd p]
    modify $ modZB ok
    return $ S.fromList $ map Pixel ok

