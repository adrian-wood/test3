PRO plot_4, start_time,end_time,minval,maxval,title,plot_type,i,OUTPUT_DIR 
  DEVICE, Set_Resolution = [640,360],Set_Colors=128

;max is 72 i.e. 6 obs per hour x 12 hours
  value=DBLARR(4,72)
  date_arr=STRARR(72)
  time_arr=STRARR(72)

;define scalar strings

  date_val='' 
  time_val = ''
  my_date_arr=''
  no_val=0
  sval1=""&  sval2=""&  sval3=""&  sval4=""
  sub_title=""
  
  ;read number of values 
    READF,1,no_val
    IF no_val NE 0 THEN BEGIN 
    ;read in values 
      FOR j=1,no_val DO BEGIN &$
        READF, 1, date_val, time_val, value1,value2,value3,value4,FORMAT="(A11,A5,4(1X,F12.3))"    &$
        date_arr(j)=date_val    &$
        time_arr(j)=time_val    &$
        value(0,j)=value1    &$
	value(1,j)=value2 &$
	value(2,j)=value3 &$
	value(3,j)=value4 &$
      ENDFOR   &$

    minval=fix(minval) 
    IF (maxval eq "MAX") THEN BEGIN 
      maxval1=MAX(value(0,1:no_val))
      maxval2=MAX(value(1,1:no_val))
      maxval3=MAX(value(2,1:no_val))
      maxval4=MAX(value(3,1:no_val))
      maxval=MAX([maxval1,maxval2,maxval3,maxval4]) 
    ENDIF ELSE maxval=fix(maxval)

    IF (minval eq 0. and maxval eq 0.) THEN maxval=1.

      my_date_arr = STR_TO_DT(date_arr(1:no_val),time_arr(1:no_val),Date_Fmt=2,Time_Fmt=-1)

      !y.style=1
      !y.range=[minval,maxval]

      plot, [start_time, end_time],[minval,maxval], /nodata,/YNozero, Title=title, $
             XTitle='Time',/box, Max_level=2, DT_Range=[start_time.julian,end_time.julian], YTickformat='(F7.1)'

      oplot, my_date_arr,value(0,1:no_val), color=0,psym=1,Symsize=0.8
      oplot, my_date_arr,value(1,1:no_val), color=4,psym=4,Symsize=0.8
      oplot, my_date_arr,value(2,1:no_val), color=3,psym=5,Symsize=0.8
      oplot, my_date_arr,value(3,1:no_val), color=2,psym=6,Symsize=0.8 

; plot legend
      !P.Charsize=1.
      label1=['Cloud Layer 1','Cloud Layer 2','Cloud Layer 3','Cloud Layer 4']
      psym_style1=[1,4,5,6]
      colors1=[0,4,3,2]
      xr=((!x.crange(1)-!x.crange(0))*1.0)+!x.crange(0)
      yr=((!y.crange(1)-!y.crange(0))*1.2)+!y.crange(0)
      delta=(!y.crange(1)-!y.crange(0))*0.06
      ukmo_legend,label1,xcoord=xr,ycoord=yr,delta_y=delta,colors=colors1,psyms=psym_style1,/one_sym

; read in cover info
      READF, 1,date_val, time_val, sval1,sval2,sval3,sval4,FORMAT="(A11,A5,1X,4A13)"
    sub_title="At "+time_val+" clouds groups "+sval1+sval2+sval3+sval4
    xlabel=!x.crange(0)+1.2*(!x.crange(1)-!x.crange(0))
    ylabel=!y.crange(0)-0.3*(!y.crange(1)-!y.crange(0))
    xyouts,xlabel,ylabel,sub_title,charsize=1.2,charthick=1,/noclip,alignment=1.
  
    ENDIF ELSE BEGIN
      plot, [start_time, end_time],[0,1],yrange=[0,1],Ystyle=1 , /nodata,/YNozero, $  
        Title=title, XTitle='Time',/box, Max_level=2, DT_Range=[start_time.julian,end_time.julian]
      READF, 1,date_val, time_val, sval1,sval2,sval3,sval4,FORMAT="(A11,A5,1X,4A13)"
    ENDELSE

    filename=Output_dir+STRTRIM(STRING(I)+".gif",1)

    IMAGE_GRAB,filename,/overwrite,/PNG
  DEVICE, Set_Resolution = [400,225],Set_Colors=128

END
