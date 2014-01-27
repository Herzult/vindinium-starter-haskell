module Vindinium.Play
        ( playTraining
        , playArena
        )
    where

import Vindinium.Types
import Vindinium.Api

playTraining :: Maybe Int -> Maybe Board -> Bot -> Vindinium State
playTraining mt mb b = startTraining mt mb >>= playLoop b

playArena :: Bot -> Vindinium State
playArena b = startArena >>= playLoop b

playLoop :: Bot -> State -> Vindinium State
playLoop bot state =
    if (gameFinished . stateGame) state
        then return state
        else do
            newState <- bot state >>= move state
            playLoop bot newState
