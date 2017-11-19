main = do
    putStrLn "What's your weight (lbs)?"
    weightLbs <- getLine
    putStrLn "What's your height (inches)?"
    heightInchs <- getLine
    let weightInt = (read weightLbs)
        weightKg = weightInt /2.2046226218
        heightInt = read heightInchs
        heightCm = heightInt * 2.54
        heightM = heightCm / 100
        bmi = weightKg / (heightM * heightM)
        health = getBmiValue bmi
    putStrLn health

getBmiValue bmi
    | bmi <= 18.5 = "You're underweight"  
    | bmi <= 25.0 = "You're normal"  
    | bmi <= 30.0 = "You're overweight"  
    | otherwise   = "You're obese"  
    