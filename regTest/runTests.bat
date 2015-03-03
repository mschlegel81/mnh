@del *.log.1
@cd ..
@echo 99bottles.mnh        
@mnh_console demos\99bottles.mnh        > regTest\99bottles.log.1
@echo anagram.mnh          
@mnh_console demos\anagram.mnh          > regTest\anagram.log.1
@echo digitSquareSum.mnh   
@mnh_console demos\digitSquareSum.mnh   > regTest\digitSquareSum.log.1
@echo fibonacci.mnh        
@mnh_console demos\fibonacci.mnh        > regTest\fibonacci.log.1
@echo fibonacciWord.mnh    
@mnh_console demos\fibonacciWord.mnh    > regTest\fibonacciWord.log.1
@echo hailstone.mnh        
@mnh_console demos\hailstone.mnh        > regTest\hailstone.log.1
@echo hofstadterConway.mnh 
@mnh_console demos\hofstadterConway.mnh > regTest\hofstadterConway.log.1
@echo huffmann.mnh         
@mnh_console demos\huffmann.mnh         > regTest\huffmann.log.1
@echo josephus.mnh         
@mnh_console demos\josephus.mnh         > regTest\josephus.log.1
@echo quine.mnh            
@mnh_console demos\quine.mnh            > regTest\quine.log.1
@echo quine2.mnh           
@mnh_console demos\quine2.mnh           > regTest\quine2.log.1
@echo rk4.mnh              
@mnh_console demos\rk4.mnh              > regTest\rk4.log.1
@echo root.mnh             
@mnh_console demos\root.mnh             > regTest\root.log.1
@echo setConsolidation.mnh 
@mnh_console demos\setConsolidation.mnh > regTest\setConsolidation.log.1
@echo shannonEntropy.mnh   
@mnh_console demos\shannonEntropy.mnh   > regTest\shannonEntropy.log.1
@echo sierpinski.mnh       
@mnh_console demos\sierpinski.mnh       > regTest\sierpinski.log.1
@echo ulam.mnh             
@mnh_console demos\ulam.mnh             > regTest\ulam.log.1
@echo vampire.mnh          
@mnh_console demos\vampire.mnh          > regTest\vampire.log.1
@echo wordCount.mnh        
@mnh_console demos\wordCount.mnh        > regTest\wordCount.log.1
@echo y.mnh                
@mnh_console demos\y.mnh                > regTest\y.log.1
@echo '-----------------Comparison report start----------------------'
@mnh_console regTest\compareLogs.mnh
@echo '-----------------Comparison report end------------------------'
@cd regTest
