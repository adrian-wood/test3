PRO plot_1, start_time,end_time,minval,maxval,title,plot_type,i,OUTPUT_DIR 
;max is 72 i.e. 6 obs per hour x 12 hours
  value=DBLARR(72)
  date_arr=STRARR(72)
  time_arr=STRARR(72)

;define scalar strings

  date_val='' 
  time_val = ''
  value1=''
  my_date_arr=''
  no_val=0

  ;read number of values 
    READF,1,no_val
    IF no_val NE 0 THEN BEGIN 
    ;read in values 
      FOR j=1,no_val DO BEGIN &$
        READF, 1, date_val, time_val, value1,FORMAT="(A11,A5,F11.3)"    &$
        date_arr(j)=date_val    &$
        time_arr(j)=time_val    &$
        value(j)=value1    &$
      ENDFOR   &$

    IF (minval eq "MIN") THEN minval=MIN(value(1:no_val)) ELSE minval=fix(minval)
    IF (maxval eq "MAX") THEN maxval=MAX(value(1:no_val)) ELSE maxval=fix(maxval)
    IF (maxval eq 0.) THEN maxval=1.
   
    my_date_arr = STR_TO_DT(date_arr(1:no_val),time_arr(1:no_val),Date_Fmt=2,Time_Fmt=-1)
   
    IF (plot_type eq 2 or plot_type eq 3) THEN !y.style=2 ELSE !y.style=1
    !y.range=[minval,maxval] 
   
      plot, [start_time, end_time],value, /nodata,/YNozero, Title=title, $
             XTitle='Time',/box, Max_level=2, DT_Range=[start_time.julian,end_time.julian], YTickformat='(F7.1)' 

      IF plot_type eq 3 THEN BEGIN
        oplot, my_date_arr,value(1:no_val), color=2,psym=1 
      ENDIF ELSE BEGIN
        oplot, my_date_arr,value(1:no_val), color=2
      ENDELSE
      
      xlabel=!x.crange(0)+1.*(!x.crange(1)-!x.crange(0))
      ylabel=!y.crange(0)+1.2*(!y.crange(1)-!y.crange(0))
      xyouts,xlabel,ylabel,string(value(no_val),format='(f9.1)'),charsize=1.2,charthick=1,color=0,/noclip,alignment=1.

    ENDIF ELSE BEGIN   
      plot, [start_time, end_time],[0,1],yrange=[0,1],Ystyle=1 , /nodata,/YNozero, $  
        Title=title, XTitle='Time',/box, Max_level=2, DT_Range=[start_time.julian,end_time.julian] 
    ENDELSE   

    filename=Output_dir+STRTRIM(STRING(I)+".gif",1) 

    IMAGE_GRAB,filename,/overwrite,/PNG 

END
