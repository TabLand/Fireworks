module MyAnimation where
import Animation

--rocket body
body::Animation
body = translate(ever(395,225))
			(withGenPaint(ever red) (ever 1)
				(rect (ever 10) (ever 150)))
				
background :: Animation
background = translate (ever (0,0))
			(withGenPaint (ever white) (ever 1)
				(rect (ever 800) (ever 600)))
--each individual spark body				
spark :: Double->Animation
--translate to top of dynamite
spark time = sparkColour (time/100) (rect (ever 5) (ever 10))

--list of (pseemingly) random spark objects
sparks:: [Animation]
sparks = [ translate (sparkJump (x) (((pRHelper100 x)-50)*3) ((pRHelper100 x)+50)) ((sparkFlip (x+50) (spark (x+50)))) | x<-(pRListHelper100 20)]

--the translation array made just for spark animations
sparkJump:: Double->Double->Double->Varying Point
sparkJump x y time= (repeatSmooth center [((0),center),((time/100),centerP(x, y))])

--the rotation for each spark,
sparkFlip:: Double -> Animation->Animation
sparkFlip time spark_instance = (rotate (spinner (time/50)) spark_instance)

--the colour transformation for each spark
sparkColour:: Double->Animation -> Animation
sparkColour time spark_instance = withGenPaint (repeatSmooth yellow [((0),yellow),(time,orange)]) (ever 1) spark_instance

--the definition for the colour orange
orange :: Colour
orange = rgb 1 0.647058824 0

--the center point for my rocket and overall spinning animation
center :: (Double,Double)
center = (400.0,225.0)

-- takes 2 points, returns the summed point
pointPlus :: Point->Point->Point
pointPlus (x,y) (x1,y1) = (x1+x,y1+y)

--helper function, to add a point to a center
centerP :: Point->Point
centerP po = pointPlus center po

--ripped from your lectures Just because useful
negatePoint :: Point->Point
negatePoint (x,y) = (-x,-y)

--my rocket
rocket::Animation
rocket = (combine sparks `plus` body)

--timing value changed from here
rocketSpinTime :: Double
rocketSpinTime = 50

--number of spins needed for spiral path and rocket path
spin :: Double
spin = 5

--translation transformation for the rocket according the spiral path
setPathRocket :: Animation -> Animation
setPathRocket rocket = translate (cycleSmooth (rocketSpinTime/(360+1)) (spiralPath (360.0*spin))) rocket

--aligning the rocket perpendicular to the spiral path
setSpinningRocket :: Animation -> Animation
setSpinningRocket rocket = (translate (ever center) 
			     (rotate (spinner (rocketSpinTime/spin))
			     --scale down
			       (scale (repeatSmooth (1,1) [(0,(1,1)),(rocketSpinTime,(0.1,0.1))]) 
			       --align to perpendicular
				(rotate (ever (90)) 
				  (translate (ever(negatePoint (centerP (0.0,75.0))))  rocket)))))

--the total animation
total :: Animation
total = background `plus` (spiralPoly (spiralPath (360*spin)) 2)  `plus` (setPathRocket (setSpinningRocket rocket))

--cause you asked for it
picture :: Animation
picture = total

--------------------------- My own pseudo random number generator------------------------------------------
pRandom :: Double->Double->Double->Double
pRandom seed min max = min + (pRSG seed) `modulus`  (max-min )

--pseudo Random Seed Generator
--Initial testing showed euler to the power three helped create a somewhat random sequence. Do not call me out on this
--seed feeds into itself
pRSG :: Double->Double
pRSG seed = ((seed) ^ 2) * (euler^3)

--helper function for creating a list of 100 random doubles
pRListHelper100::Double->[Double]
pRListHelper100 seed = pRList seed (-50) 50 100

--helper function to generate a random value between -50 and 50
pRHelper100 :: Double -> Double
pRHelper100 seed = pRandom seed (-50) 50

--Modulus for doubles, cause the haskell mod function sucks
modulus :: Double -> Double->Double
modulus a b | (a<b) = a
     	    | otherwise = a - fromIntegral (floor(a/b))*b 

--Too sleep deprived to find the haskell version of this, so value ripped off microsoft excel
euler :: Double
euler = 2.7182818285

--creates a list of random numbers
pRList :: Double->Double->Double->Double->[Double]
pRList seed min max size | (size<=0) = [pRandom seed min max]
		         | otherwise = [pRandom (pRandom seed min max) min max] ++  pRList (pRandom seed min max) min max (size-1)

--generates a spiral path using the polar equation x=sin angle, y = cos angle, where angle is the angular rotation in degrees
spiralPath :: Double->[Point]
spiralPath number = [(t/5 * sind t,t/5 * cosd t) | t<-(reverse [1,(spin+1)..number])]

--too sleepy to find haskell version of this
--degree to radians
d2r :: Double -> Double
d2r degrees = degrees / 180  * pi

--too sleepy to find haskell version of this
--sine for degrees
sind :: Double -> Double
sind degrees = sin (d2r degrees)

--too sleepy to find haskell version of this
--sine for degrees
cosd :: Double -> Double
cosd degrees = cos (d2r degrees)


--creates a list of grey rectangles to map out the spiral path
spiralPoly :: [Point]->Double->Animation
spiralPoly spiral side | ((length spiral)>0) =  
				(translate (ever (pointPlus center (head spiral))) 
				  (dot side) `plus` (spiralPoly (tail (spiral)) side))
	               | otherwise = (dot side)
	               
--tried to make lines using polygons. DID NOT WORK!!!
dot :: Double->Animation
dot side = (withGenPaint (ever gray) (ever 1) (rect (ever side) (ever side)))


-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------
----------------------------------------------REMEMBER TO REMOVE THIS!!!!!---------------------------------------
----------------------------------------------REMEMBER TO REMOVE THIS!!!!!---------------------------------------
-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

test :: IO ()
test = writeFile "test.svg" (svg 800 600 picture)
