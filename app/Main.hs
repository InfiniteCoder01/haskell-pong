{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Data.Fixed
import Raylib.Core
  ( initWindow, setTargetFPS, windowShouldClose, closeWindow,
    getRenderWidth, getRenderHeight, getFrameTime, getRandomValue,
    isKeyDown, isKeyPressed,
    clearBackground
  )
import Raylib.Types.Core (KeyboardKey(..))
import Raylib.Core.Text (drawFPS, drawText, measureText)
import Raylib.Types (pattern Vector2, Vector2, vector2'x, vector2'y)
import Raylib.Util (raylibApplication, drawing, WindowResources)
import Raylib.Core.Shapes (drawRectangleV, drawCircleV)
import Raylib.Util.Colors (black, white)

for :: Monad m => (a -> m ()) -> [a] -> m ()
for c xs = foldl (\a b -> a >> b) (pure ()) (map c xs)

data AppState = AppState {
  window :: WindowResources,

  p1y :: Float,
  p1v :: Float,
  p1s :: Int,

  p2y :: Float,
  p2v :: Float,
  p2s :: Int,

  ball :: Vector2,
  ballDir :: Vector2,

  ai :: AI
}

data AI = Simple | Pred | Crazy deriving (Eq, Enum, Bounded, Show)

startup :: IO AppState
startup = do
  window <- initWindow 640 480 "Pong"
  setTargetFPS 60

  return AppState {
    window,

    p1y = 0,
    p1v = 0,
    p1s = 0,

    p2y = 0,
    p2v = 0,
    p2s = 0,

    ball = Vector2 (-1) (-1),
    ballDir = Vector2 0 0,

    ai = Simple
  }

mainLoop :: AppState -> IO AppState
mainLoop state@AppState { p1y, p1v, p1s, p2y, p2v, p2s, ball, ballDir, ai } = do
  iwidth <- getRenderWidth
  iheight <- getRenderHeight
  let (width, height) = ((fromIntegral iwidth), (fromIntegral iheight))
  let paddleSize = Vector2 (height / 64) (height / 16)
  let ballRadius = height / 96.0
  let paddlePadding = height / 16.0

  ball <- return $ if ball == Vector2 (-1) (-1)
    then (Vector2 width height / Vector2 2.0 2.0)
    else ball

  ballDir <- do
    if ballDir == Vector2 0 0
    then do
      dx <- getRandomValue 0 1
      dy <- getRandomValue (-128) 128
      return $ Vector2
        (fromIntegral dx * 2 - 1)
        (fromIntegral dy / 128.0 * 0.8)
    else return $ ballDir

  ai <- do
    next <- isKeyPressed KeyOne
    return $ if next
      then if ai == maxBound
             then minBound
             else succ ai
      else ai
  
  
  drawing $ do
    clearBackground black
    let lineLen = width / 40
        lineWidth = width / 160
        spacing = lineLen / 2
      in do
        for (\x -> do
          drawRectangleV (Vector2 x 0) (Vector2 lineLen lineWidth) white
          drawRectangleV (Vector2 x (height - lineWidth)) (Vector2 lineLen lineWidth) white)
          [0, spacing + lineLen..width]
        for (\y -> do
          drawRectangleV (Vector2 ((width - lineWidth) / 2) y) (Vector2 lineWidth lineLen) white)
          [0, spacing + lineLen..height]
    drawFPS 10 10
    drawRectangleV (Vector2 paddlePadding p1y) paddleSize white
    drawRectangleV (Vector2 (width - paddlePadding - vector2'x paddleSize) p2y) paddleSize white
    drawCircleV ball ballRadius white

    let fontSize = round $ height / 16.0
    textWidth <- measureText (show ai) fontSize
    drawText (show ai) (iwidth - textWidth - 10) 10 fontSize white
    fontSize <- return $ fontSize * 2
    textWidth <- measureText (show p1s) fontSize
    drawText (show p1s) (round (width / 4 - fromIntegral textWidth / 2) - 10) 10 fontSize white
    textWidth <- measureText (show p2s) fontSize
    drawText (show p2s) (round (width / 4 * 3 - fromIntegral textWidth / 2) - 10) 10 fontSize white


  deltaTime <- getFrameTime
  let paddleSpeed = height
  let paddleAISpeed = paddleSpeed * 1.0
  let ballSpeed = height * 0.4 * deltaTime
  let paddleBallVelocityTransfer = 1.4
  let paddleBounds = \x v -> let vv = v * deltaTime in if x + vv < 0 then (0, 0) else if x + vv > height - vector2'y paddleSize then (height - vector2'y paddleSize, 0) else (x + vv, v)
  let paddleAccBlend = 1.0 - 0.05 ** deltaTime
  let ballBounceK = -1.1

  -- Left Paddle (player)
  up <- isKeyDown KeyW
  down <- isKeyDown KeyS
  let tp1v = (fromIntegral $ fromEnum down - fromEnum up) * paddleSpeed
  p1v <- return $ p1v + (tp1v - p1v) * paddleAccBlend
  (p1y, p1v) <- return $ paddleBounds p1y p1v

  -- Right Paddle (AI)
  let (ballCrossY, ballCrossT) = if vector2'x ballDir > 0
        then let
          bx = vector2'x ball + ballRadius
          px = width - paddlePadding - vector2'x paddleSize
          xDistance = px - bx
          ballIntersectionY = vector2'y ball + xDistance / (vector2'x ballDir) * vector2'y ballDir
          screenSpaceBallIntersectionY = Data.Fixed.mod' ballIntersectionY height
        in
          ((if odd (floor (ballIntersectionY / height) :: Int)
          then height - screenSpaceBallIntersectionY
          else screenSpaceBallIntersectionY), xDistance / vector2'x ballDir / ballSpeed * deltaTime)
        else (0/0, 0/0)
  
  let target = case ai of
        Simple -> vector2'y ball
        Pred -> ballCrossY
        Crazy -> if ballCrossT < abs (ballCrossY - p2y - vector2'y paddleSize / 2) / paddleAISpeed * 1.8 then ballCrossY - (p2y - ballCrossY) else 0/0
  let delta = if isNaN target then 0 else target - p2y - vector2'y paddleSize / 2
  let tp2v = max (-paddleAISpeed) $ min paddleAISpeed $ delta / deltaTime
  p2v <- return $ p2v + (tp2v - p2v) * paddleAccBlend
  (p2y, p2v) <- return $ paddleBounds p2y p2v

  -- Ball
  ball <- return $ ball + ballDir * Vector2 ballSpeed ballSpeed
  (ball, ballDir) <- return $ if vector2'y ball + ballRadius > height
    then (Vector2 (vector2'x ball) (height - ballRadius), ballDir * Vector2 1.0 (-1.0))
    else (ball, ballDir)
  (ball, ballDir) <- return $ if vector2'y ball - ballRadius < 0
    then (Vector2 (vector2'x ball) ballRadius, ballDir * Vector2 1.0 (-1.0))
    else (ball, ballDir)

  -- Ball VS Paddle collisions
  (ball, ballDir) <- return $ if
    (vector2'y ball - ballRadius < p1y + vector2'y paddleSize
    && vector2'y ball + ballRadius > p1y
    && vector2'x ball - ballRadius < paddlePadding + vector2'x paddleSize
    && vector2'x ball + ballRadius > paddlePadding)
    then (Vector2 (paddlePadding + vector2'x paddleSize + ballRadius) $ vector2'y ball, Vector2 (vector2'x ballDir * ballBounceK) $ p1v / paddleSpeed * paddleBallVelocityTransfer)
    else (ball, ballDir)

  (ball, ballDir) <- return $ if
    (vector2'y ball - ballRadius < p2y + vector2'y paddleSize
    && vector2'y ball + ballRadius > p2y
    && vector2'x ball - ballRadius < width - paddlePadding
    && vector2'x ball + ballRadius > width - paddlePadding - vector2'x paddleSize)
    then (Vector2 (width - paddlePadding - vector2'x paddleSize - ballRadius) $ vector2'y ball, Vector2 (vector2'x ballDir * ballBounceK) $ p2v / paddleSpeed * paddleBallVelocityTransfer)
    else (ball, ballDir)

  -- Sides
  (ball, ballDir, p1s) <- return $ if vector2'x ball + ballRadius > width
    then (Vector2 (-1) (-1), Vector2 0 0, p1s + 1)
    else (ball, ballDir, p1s)
  (ball, ballDir, p2s) <- return $ if vector2'x ball - ballRadius < 0
    then (Vector2 (-1) (-1), Vector2 0 0, p2s + 1)
    else (ball, ballDir, p2s)

  ball <- return $ if (ball == (Vector2 (-1) (-1))) then ((Vector2 width height) / (Vector2 2.0 2.0)) else ball
  return state {
    p1y = p1y,
    p1v = p1v,
    p1s = p1s,
    
    p2y = p2y,
    p2v = p2v,
    p2s = p2s,
    
    ball = ball,
    ballDir = ballDir,

    ai = ai
  }

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown AppState { window } = do
  closeWindow (Just window)

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
