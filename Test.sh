App=tom
make ${App}
./${App} -c ${App}0.in >${App}0.ex
./${App} -c ${App}1.in >${App}1.ex
./${App} -c ${App}2.in >${App}2.ex
./${App} -c ${App}3.in >${App}3.ex
./${App} -c ${App}4.in >${App}4.ex
./${App} -c ${App}5.in >${App}5.ex
./${App} -c ${App}6.in >${App}6.ex
./${App} -c ${App}7.in >${App}7.ex
./${App} -c ${App}8.in >${App}8.ex
./${App} -s ${App}2.in <${App}2x.in >${App}2x.ex
diff ${App}0.ex Ref/${App}0.ex
diff ${App}1.ex Ref/${App}1.ex
diff ${App}2.ex Ref/${App}2.ex
diff ${App}3.ex Ref/${App}3.ex
diff ${App}4.ex Ref/${App}4.ex
diff ${App}5.ex Ref/${App}5.ex
diff ${App}6.ex Ref/${App}6.ex
diff ${App}7.ex Ref/${App}7.ex
diff ${App}8.ex Ref/${App}8.ex
diff ${App}2x.ex Ref/${App}2x.ex
rm ${App}{0,1,2,3,4,5,6,7,8,2x}.ex
