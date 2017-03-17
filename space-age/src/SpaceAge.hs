module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            deriving (Eq, Ord, Show)

planets :: [Planet]
planets = [Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune]

periods :: [Float]
periods = [0.2408467, 0.61519726, 1.0, 1.8808158, 11.862615, 29.447498, 84.016846, 164.79132]

earthYearSeconds :: Float
earthYearSeconds = 31557600.0

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / orbitSeconds
  where (Just period) = lookup planet (zip planets periods)
        orbitSeconds  = earthYearSeconds * period
