@rem mnh format@("@echo %I3% %I3% %I3% %I3% %I3% %I3% %I3% %I3% %I3% %I3%"|each(i,[1..10],i*[1..10]))
@echo   1   2   3   4   5   6   7   8   9  10
@echo   2   4   6   8  10  12  14  16  18  20
@echo   3   6   9  12  15  18  21  24  27  30
@echo   4   8  12  16  20  24  28  32  36  40
@echo   5  10  15  20  25  30  35  40  45  50
@echo   6  12  18  24  30  36  42  48  54  60
@echo   7  14  21  28  35  42  49  56  63  70
@echo   8  16  24  32  40  48  56  64  72  80
@echo   9  18  27  36  45  54  63  72  81  90
@echo  10  20  30  40  50  60  70  80  90 100
@rem mnh format@("@echo %I3% %I3% %I3% %I3% %I3% %I3% %I3% %I3% %I3% %I3%"|each(i,[1..10],i+[1..10]))
@echo   2   3   4   5   6   7   8   9  10  11
@echo   3   4   5   6   7   8   9  10  11  12
@echo   4   5   6   7   8   9  10  11  12  13
@echo   5   6   7   8   9  10  11  12  13  14
@echo   6   7   8   9  10  11  12  13  14  15
@echo   7   8   9  10  11  12  13  14  15  16
@echo   8   9  10  11  12  13  14  15  16  17
@echo   9  10  11  12  13  14  15  16  17  18
@echo  10  11  12  13  14  15  16  17  18  19
@echo  11  12  13  14  15  16  17  18  19  20
@rem mnh 2*3
6
@rem nomnh
@echo this is the old rest
@rem mnh "This is the new rest"
This is the new rest
