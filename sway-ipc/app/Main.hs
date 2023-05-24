module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (handle, SomeException)
import Control.Monad ((>=>))

import Network.Sway.IPC qualified as Sway

main :: IO ()
main = do
  putStrLn "Hello, World!"

  putStrLn "starting sway ipc"
  ipc <- Sway.startClient

  putStrLn "running handle"
  handle (\(x :: SomeException) -> Sway.stopClient ipc *> print x) do

    putStrLn "running command"
    reply <- Sway.runCommand ipc $ Sway.RunCommand "exec whoami"
    putStrLn $ "reply: " <> show reply

    putStrLn "running simple queries"
    mapM_ (print *> Sway.runCommand ipc >=> print)
      [ Sway.GetWorkspaces
      , Sway.GetOutputs
      , Sway.GetTree
      , Sway.GetMarks
      , Sway.GetVersion
      , Sway.GetBindingModes
      , Sway.GetConfig
      , Sway.GetBindingState
      ]

    putStrLn "subscribing to events"
    Sway.subscribe ipc [Sway.WindowEventType, Sway.WorkspaceEventType] $ const print

    threadDelay 10_000_000
    Sway.stopClient ipc
