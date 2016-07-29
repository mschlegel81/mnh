@echo off
@rem files('demos/*.mnh').union(files('packages/*.mnh')).each(file,file.matches('htmlFunctionPlotter|timer') ? void : file).each(demo,'echo. >> D:\heaptrace_per_demo.txt','echo '&demo&' >> D:\heaptrace_per_demo.txt','echo '&demo,'echo. >> D:\heaptrace_per_demo.txt','%1 -quiet '&demo.splitFileName['expanded']&(demo.matches('24\.mnh') ?' demo' : demo.matches('diff\.mnh') ?' test' :'')&' >> D:\heaptrace_per_demo.txt 2>&1').join("\n").print;
echo. >> D:\heaptrace_per_demo.txt
echo demos/100doors.mnh >> D:\heaptrace_per_demo.txt
echo demos/100doors.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/100doors.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/12_statements.mnh >> D:\heaptrace_per_demo.txt
echo demos/12_statements.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/12_statements.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/24.mnh >> D:\heaptrace_per_demo.txt
echo demos/24.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/24.mnh demo >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/3body.mnh >> D:\heaptrace_per_demo.txt
echo demos/3body.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/3body.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/99bottles.mnh >> D:\heaptrace_per_demo.txt
echo demos/99bottles.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/99bottles.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/ackermann.mnh >> D:\heaptrace_per_demo.txt
echo demos/ackermann.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/ackermann.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/aks.mnh >> D:\heaptrace_per_demo.txt
echo demos/aks.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/aks.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/aliquotSequence.mnh >> D:\heaptrace_per_demo.txt
echo demos/aliquotSequence.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/aliquotSequence.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/anagram.mnh >> D:\heaptrace_per_demo.txt
echo demos/anagram.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/anagram.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/asciiCalendar.mnh >> D:\heaptrace_per_demo.txt
echo demos/asciiCalendar.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/asciiCalendar.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/balancedBrackets.mnh >> D:\heaptrace_per_demo.txt
echo demos/balancedBrackets.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/balancedBrackets.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/barnsleyFern.mnh >> D:\heaptrace_per_demo.txt
echo demos/barnsleyFern.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/barnsleyFern.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/benford.mnh >> D:\heaptrace_per_demo.txt
echo demos/benford.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/benford.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/bestShuffle.mnh >> D:\heaptrace_per_demo.txt
echo demos/bestShuffle.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/bestShuffle.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/clock.mnh >> D:\heaptrace_per_demo.txt
echo demos/clock.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/clock.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/concatInts.mnh >> D:\heaptrace_per_demo.txt
echo demos/concatInts.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/concatInts.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/continuedFractions.mnh >> D:\heaptrace_per_demo.txt
echo demos/continuedFractions.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/continuedFractions.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/conventions.mnh >> D:\heaptrace_per_demo.txt
echo demos/conventions.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/conventions.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/countTheCoins.mnh >> D:\heaptrace_per_demo.txt
echo demos/countTheCoins.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/countTheCoins.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/dice7fromDice5.mnh >> D:\heaptrace_per_demo.txt
echo demos/dice7fromDice5.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/dice7fromDice5.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/diff.mnh >> D:\heaptrace_per_demo.txt
echo demos/diff.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/diff.mnh test >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/digitSquareSum.mnh >> D:\heaptrace_per_demo.txt
echo demos/digitSquareSum.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/digitSquareSum.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/ethiopian.mnh >> D:\heaptrace_per_demo.txt
echo demos/ethiopian.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/ethiopian.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/fibonacci.mnh >> D:\heaptrace_per_demo.txt
echo demos/fibonacci.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/fibonacci.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/fibonacciWord.mnh >> D:\heaptrace_per_demo.txt
echo demos/fibonacciWord.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/fibonacciWord.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/findDuplicates.mnh >> D:\heaptrace_per_demo.txt
echo demos/findDuplicates.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/findDuplicates.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/fractalPlant.mnh >> D:\heaptrace_per_demo.txt
echo demos/fractalPlant.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/fractalPlant.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/fractran.mnh >> D:\heaptrace_per_demo.txt
echo demos/fractran.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/fractran.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/gcd.mnh >> D:\heaptrace_per_demo.txt
echo demos/gcd.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/gcd.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/gol.mnh >> D:\heaptrace_per_demo.txt
echo demos/gol.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/gol.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/grep.mnh >> D:\heaptrace_per_demo.txt
echo demos/grep.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/grep.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/hailstone.mnh >> D:\heaptrace_per_demo.txt
echo demos/hailstone.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/hailstone.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/hamming.mnh >> D:\heaptrace_per_demo.txt
echo demos/hamming.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/hamming.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/harshad.mnh >> D:\heaptrace_per_demo.txt
echo demos/harshad.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/harshad.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/hash.mnh >> D:\heaptrace_per_demo.txt
echo demos/hash.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/hash.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/heart.mnh >> D:\heaptrace_per_demo.txt
echo demos/heart.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/heart.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/helloWorld.mnh >> D:\heaptrace_per_demo.txt
echo demos/helloWorld.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/helloWorld.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/helloWorldHttp.mnh >> D:\heaptrace_per_demo.txt
echo demos/helloWorldHttp.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/helloWorldHttp.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/hereDoc.mnh >> D:\heaptrace_per_demo.txt
echo demos/hereDoc.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/hereDoc.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/hofstadterConway.mnh >> D:\heaptrace_per_demo.txt
echo demos/hofstadterConway.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/hofstadterConway.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/hofstadter_sequences.mnh >> D:\heaptrace_per_demo.txt
echo demos/hofstadter_sequences.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/hofstadter_sequences.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/huffmann.mnh >> D:\heaptrace_per_demo.txt
echo demos/huffmann.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/huffmann.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/josephus.mnh >> D:\heaptrace_per_demo.txt
echo demos/josephus.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/josephus.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/kochCurve.mnh >> D:\heaptrace_per_demo.txt
echo demos/kochCurve.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/kochCurve.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/lastSundays.mnh >> D:\heaptrace_per_demo.txt
echo demos/lastSundays.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/lastSundays.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/levenshtein.mnh >> D:\heaptrace_per_demo.txt
echo demos/levenshtein.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/levenshtein.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/lindenmayer.mnh >> D:\heaptrace_per_demo.txt
echo demos/lindenmayer.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/lindenmayer.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/ludic.mnh >> D:\heaptrace_per_demo.txt
echo demos/ludic.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/ludic.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/magicSquare.mnh >> D:\heaptrace_per_demo.txt
echo demos/magicSquare.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/magicSquare.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/maze.mnh >> D:\heaptrace_per_demo.txt
echo demos/maze.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/maze.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/memoization.mnh >> D:\heaptrace_per_demo.txt
echo demos/memoization.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/memoization.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/monteCarloPi.mnh >> D:\heaptrace_per_demo.txt
echo demos/monteCarloPi.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/monteCarloPi.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/opTest.mnh >> D:\heaptrace_per_demo.txt
echo demos/opTest.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/opTest.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/pascal.mnh >> D:\heaptrace_per_demo.txt
echo demos/pascal.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/pascal.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/pernicousNumbers.mnh >> D:\heaptrace_per_demo.txt
echo demos/pernicousNumbers.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/pernicousNumbers.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/piPlots.mnh >> D:\heaptrace_per_demo.txt
echo demos/piPlots.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/piPlots.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/pythagoras.mnh >> D:\heaptrace_per_demo.txt
echo demos/pythagoras.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/pythagoras.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/quine.mnh >> D:\heaptrace_per_demo.txt
echo demos/quine.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/quine.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/quine2.mnh >> D:\heaptrace_per_demo.txt
echo demos/quine2.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/quine2.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/range_extract_expand.mnh >> D:\heaptrace_per_demo.txt
echo demos/range_extract_expand.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/range_extract_expand.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/rk4.mnh >> D:\heaptrace_per_demo.txt
echo demos/rk4.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/rk4.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/roman.mnh >> D:\heaptrace_per_demo.txt
echo demos/roman.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/roman.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/root.mnh >> D:\heaptrace_per_demo.txt
echo demos/root.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/root.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/rotatingCube.mnh >> D:\heaptrace_per_demo.txt
echo demos/rotatingCube.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/rotatingCube.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/selfRefSeq.mnh >> D:\heaptrace_per_demo.txt
echo demos/selfRefSeq.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/selfRefSeq.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/semordnilap.mnh >> D:\heaptrace_per_demo.txt
echo demos/semordnilap.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/semordnilap.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/setConsolidation.mnh >> D:\heaptrace_per_demo.txt
echo demos/setConsolidation.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/setConsolidation.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/shannonEntropy.mnh >> D:\heaptrace_per_demo.txt
echo demos/shannonEntropy.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/shannonEntropy.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/sierpinskiCarpet.mnh >> D:\heaptrace_per_demo.txt
echo demos/sierpinskiCarpet.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/sierpinskiCarpet.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/sierpinskiCurve.mnh >> D:\heaptrace_per_demo.txt
echo demos/sierpinskiCurve.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/sierpinskiCurve.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/sierpinskiTriangle.mnh >> D:\heaptrace_per_demo.txt
echo demos/sierpinskiTriangle.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/sierpinskiTriangle.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/stringFib.mnh >> D:\heaptrace_per_demo.txt
echo demos/stringFib.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/stringFib.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/subGen.mnh >> D:\heaptrace_per_demo.txt
echo demos/subGen.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/subGen.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/sudoku.mnh >> D:\heaptrace_per_demo.txt
echo demos/sudoku.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/sudoku.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/task.mnh >> D:\heaptrace_per_demo.txt
echo demos/task.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/task.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/travellingSalesmanProblem.mnh >> D:\heaptrace_per_demo.txt
echo demos/travellingSalesmanProblem.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/travellingSalesmanProblem.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/turtleGfx.mnh >> D:\heaptrace_per_demo.txt
echo demos/turtleGfx.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/turtleGfx.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/ulam.mnh >> D:\heaptrace_per_demo.txt
echo demos/ulam.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/ulam.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/unbiasedRandom.mnh >> D:\heaptrace_per_demo.txt
echo demos/unbiasedRandom.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/unbiasedRandom.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/vampire.mnh >> D:\heaptrace_per_demo.txt
echo demos/vampire.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/vampire.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/variadic.mnh >> D:\heaptrace_per_demo.txt
echo demos/variadic.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/variadic.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/weasel.mnh >> D:\heaptrace_per_demo.txt
echo demos/weasel.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/weasel.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/wordCount.mnh >> D:\heaptrace_per_demo.txt
echo demos/wordCount.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/wordCount.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/y.mnh >> D:\heaptrace_per_demo.txt
echo demos/y.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/y.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo demos/zeckendorf.mnh >> D:\heaptrace_per_demo.txt
echo demos/zeckendorf.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/demos/zeckendorf.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo packages/hash.mnh >> D:\heaptrace_per_demo.txt
echo packages/hash.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/packages/hash.mnh >> D:\heaptrace_per_demo.txt 2>&1
echo. >> D:\heaptrace_per_demo.txt
echo packages/statistics.mnh >> D:\heaptrace_per_demo.txt
echo packages/statistics.mnh
echo. >> D:\heaptrace_per_demo.txt
%1 -quiet D:/dev/mnh5/packages/statistics.mnh >> D:\heaptrace_per_demo.txt 2>&1