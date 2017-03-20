rename A Year
rename B Age
rename C mx
rename D qx
rename E ax
rename F lx
rename G dx
rename H Lx
rename I Tx
rename J ex
rename K Bx
rename L Kx
rename M Fx
mat Les=J(24,24,0)
forvalues i=1/23{
                 mat Les[`i'+1,`i']=Lx[`i'+1]/Lx[`i']
                }
forvalues j=1/23{
                 mat Les[1,`j']=Lx[1]*0.4886/2/lx[1]*(Lx[`j'] *Fx[`j'] +Lx[`j'+1]*Fx[`j'+1])/Lx[`j']
                }		
. putexcel A1=matrix(Les, names) using "\\netid.washington.edu\csde\other\desktop\yxx\Desktop\dem\dem\SWEtable.xls"
file \\netid.washington.edu\csde\other\desktop\yxx\Desktop\dem\dem\SWE.xls saved

