@if "%1"=="" %0 mnh_console
@del regTest\*.log.1
@mnh_console regTest\regTest.mnh clear_times
@echo   executing 99bottles.mnh        & @%1 demos\99bottles.mnh        > regTest\99bottles.log.1        & @mnh_console regTest\regTest.mnh 99bottles
@echo   executing ackermann.mnh        & @%1 demos\ackermann.mnh        > regTest\ackermann.log.1        & @mnh_console regTest\regTest.mnh ackermann
@echo   executing anagram.mnh          & @%1 demos\anagram.mnh          > regTest\anagram.log.1          & @mnh_console regTest\regTest.mnh anagram
@echo   executing digitSquareSum.mnh   & @%1 demos\digitSquareSum.mnh   > regTest\digitSquareSum.log.1   & @mnh_console regTest\regTest.mnh digitSquareSum
@echo   executing fibonacci.mnh        & @%1 demos\fibonacci.mnh        > regTest\fibonacci.log.1        & @mnh_console regTest\regTest.mnh fibonacci
@echo   executing fibonacciWord.mnh    & @%1 demos\fibonacciWord.mnh    > regTest\fibonacciWord.log.1    & @mnh_console regTest\regTest.mnh fibonacciWord
@echo   executing hailstone.mnh        & @%1 demos\hailstone.mnh        > regTest\hailstone.log.1        & @mnh_console regTest\regTest.mnh hailstone
@echo   executing hofstadterConway.mnh & @%1 demos\hofstadterConway.mnh > regTest\hofstadterConway.log.1 & @mnh_console regTest\regTest.mnh hofstadterConway
@echo   executing huffmann.mnh         & @%1 demos\huffmann.mnh         > regTest\huffmann.log.1         & @mnh_console regTest\regTest.mnh huffmann
@echo   executing josephus.mnh         & @%1 demos\josephus.mnh         > regTest\josephus.log.1         & @mnh_console regTest\regTest.mnh josephus
@echo   executing quine.mnh            & @%1 demos\quine.mnh            > regTest\quine.log.1            & @mnh_console regTest\regTest.mnh quine
@echo   executing quine2.mnh           & @%1 demos\quine2.mnh           > regTest\quine2.log.1           & @mnh_console regTest\regTest.mnh quine2
@echo   executing rk4.mnh              & @%1 demos\rk4.mnh              > regTest\rk4.log.1              & @mnh_console regTest\regTest.mnh rk4
@echo   executing root.mnh             & @%1 demos\root.mnh             > regTest\root.log.1             & @mnh_console regTest\regTest.mnh root
@echo   executing setConsolidation.mnh & @%1 demos\setConsolidation.mnh > regTest\setConsolidation.log.1 & @mnh_console regTest\regTest.mnh setConsolidation
@echo   executing shannonEntropy.mnh   & @%1 demos\shannonEntropy.mnh   > regTest\shannonEntropy.log.1   & @mnh_console regTest\regTest.mnh shannonEntropy
@echo   executing sierpinski.mnh       & @%1 demos\sierpinski.mnh       > regTest\sierpinski.log.1       & @mnh_console regTest\regTest.mnh sierpinski
@echo   executing ulam.mnh             & @%1 demos\ulam.mnh             > regTest\ulam.log.1             & @mnh_console regTest\regTest.mnh ulam
@echo   executing vampire.mnh          & @%1 demos\vampire.mnh          > regTest\vampire.log.1          & @mnh_console regTest\regTest.mnh vampire
@echo   executing wordCount.mnh        & @%1 demos\wordCount.mnh        > regTest\wordCount.log.1        & @mnh_console regTest\regTest.mnh wordCount
@echo   executing y.mnh                & @%1 demos\y.mnh                > regTest\y.log.1                & @mnh_console regTest\regTest.mnh y
@echo '-----------------Comparison report start----------------------'
@mnh_console regTest\regTest.mnh
@echo '-----------------Comparison report end------------------------'
@del timing.txt