{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Raylib.Core
  ( initWindow, setTargetFPS, windowShouldClose, closeWindow,
    getRenderWidth, getRenderHeight, getFrameTime, getRandomValue,
    isKeyDown,
    clearBackground
  )
import Raylib.Types.Core (KeyboardKey(..))
import Raylib.Core.Text (drawFPS)
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
  p2y :: Float,
  p2v :: Float,
  ball :: Vector2,
  ballDir :: Vector2
}

data AI = Simple | Pred | Crazy deriving (Show)

startup :: IO AppState
startup = do
  window <- initWindow 640 480 "Pong"
  setTargetFPS 60

  return AppState {
    window,
    p1y = 0,
    p1v = 0,
    p2y = 0,
    p2v = 0,
    ball = Vector2 (-1) (-1),
    ballDir = Vector2 0 0
  }

mainLoop :: AppState -> IO AppState
mainLoop state@AppState { p1y, p1v, p2y, p2v, ball, ballDir } = do
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
    drawFPS 10 20
    drawRectangleV (Vector2 paddlePadding p1y) paddleSize white
    drawRectangleV (Vector2 (width - paddlePadding - vector2'x paddleSize) p2y) paddleSize white
    drawCircleV ball ballRadius white

  deltaTime <- getFrameTime
  let paddleSpeed = height * deltaTime
  let paddleAISpeed = paddleSpeed * 0.7
  let ballSpeed = height * 0.4 * deltaTime
  let paddleBallVelocityTransfer = 1.4
  let paddleBounds = \x v -> if x + v < 0 then (0, 0) else if x + v > height - vector2'y paddleSize then (height - vector2'y paddleSize, 0) else (x + v, v)
  let paddleAccBlend = 1.0 - 0.05 ** deltaTime

  -- Left Paddle (player)
  up <- isKeyDown KeyW
  down <- isKeyDown KeyS
  let tp1v = (fromIntegral (fromEnum down - fromEnum up)) * paddleSpeed
  p1v <- return $ p1v + (tp1v - p1v) * paddleAccBlend
  (p1y, p1v) <- return $ paddleBounds p1y p1v

  -- Right Paddle (AI)
  let delta = vector2'y ball - p2y - vector2'y paddleSize / 2
  let tp2v = max (-paddleAISpeed) $ min paddleAISpeed delta
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
    then (Vector2 (paddlePadding + vector2'x paddleSize + ballRadius) $ vector2'y ball, Vector2 1.0 $ p1v / paddleSpeed * paddleBallVelocityTransfer)
    else (ball, ballDir)

  (ball, ballDir) <- return $ if
    (vector2'y ball - ballRadius < p2y + vector2'y paddleSize
    && vector2'y ball + ballRadius > p2y
    && vector2'x ball - ballRadius < width - paddlePadding
    && vector2'x ball + ballRadius > width - paddlePadding - vector2'x paddleSize)
    then (Vector2 (width - paddlePadding - vector2'x paddleSize - ballRadius) $ vector2'y ball, Vector2 (-1.0) $ p2v / paddleSpeed * paddleBallVelocityTransfer)
    else (ball, ballDir)

  -- Sides
  (ball, ballDir) <- return $ if vector2'x ball + ballRadius > width
    then (Vector2 (-1) (-1), Vector2 0 0)
    else (ball, ballDir)
  (ball, ballDir) <- return $ if vector2'x ball - ballRadius < 0
    then (Vector2 (-1) (-1), Vector2 0 0)
    else (ball, ballDir)

  ball <- return $ if (ball == (Vector2 (-1) (-1))) then ((Vector2 width height) / (Vector2 2.0 2.0)) else ball
  return state {
    p1y = p1y,
    p1v = p1v,
    p2y = p2y,
    p2v = p2v,
    ball = ball,
    ballDir = ballDir
  }

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown AppState { window } = do
  closeWindow (Just window)

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
