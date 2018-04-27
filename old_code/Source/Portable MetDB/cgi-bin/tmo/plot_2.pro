PRO plot_2, start_time,end_time,minval,maxval,title,plot_type,i,OUTPUT_DIR 

;max is 72 i.e. 6 obs per hour x 12 hours
  value=DBLARR(2,72)
  date_arr=STRARR(72)
  time_arr=STRARR(72)

;define scalar strings

  date_val='' 
  time_val = ''
  value1=''
  value2=''
  my_date_arr=''
  no_val=0

  ;read number of values 
    READF,1,no_val

    IF no_val NE 0 THEN BEGIN 
    ;read in values 
      FOR j=1,no_val DO BEGIN &$
        READF, 1, date_val, time_val, value1,value2,FORMAT="(A11,A5,1X,2F12.3)"    &$
        date_arr(j)=date_val    &$
        time_arr(j)=time_val    &$
        value(0,j)=value1    &$
	value(1,j)=value2 &$
      ENDFOR   &$

    IF (minval eq "MIN") THEN BEGIN
      minval1=MIN(value(0,1:no_val))
      minval2=MIN(value(1,1:no_val))
      minval=MIN([minval1,minval2]) 
    ENDIF ELSE minval=fix(minval) 
    IF (maxval eq "MAX") THEN BEGIN 
      maxval1=MAX(value(0,1:no_val)) 
      maxval2=MAX(value(1,1:no_val)) 
      maxval=MAX([maxval1,maxval2]) 
    ENDIF ELSE maxval=fix(maxval) 

    IF (minval eq 0. and maxval eq 0.) THEN maxval=1. 

      my_date_arr = STR_TO_DT(date_arr(1:no_val),time_arr(1:no_val),Date_Fmt=2,Time_Fmt=-1)    

      IF plot_type eq 6 THEN !y.style=2 ELSE !y.style=1
      !y.range=[minval,maxval]    
          
      plot, [start_time, end_time],[minval,maxval], /nodata, Title=title, $
             XTitle='Time',/box, Max_level=2, DT_Range=[start_time.julian,end_time.julian], YTickformat='(F7.1)'   

      IF plot_type eq 6 THEN BEGIN
        oplot, my_date_arr,value(1,1:no_val), color=4, psym=1
      ENDIF ELSE BEGIN
          oplot, my_date_arr,value(1,1:no_val), color=4
      ENDELSE
      
      oplot, my_date_arr,value(0,1:no_val), color=2   
      
      xlabel=!x.crange(0)+1.1*(!x.crange(1)-!x.crange(0)) 
      ylabel=!y.crange(0)+1.2*(!y.crange(1)-!y.crange(0)) 
      xyouts,xlabel,ylabel,string(value(0,no_val),format='(f9.1)'),charsize=1.2,charthick=1,color=2,/noclip,alignment=1. 
      xlabel=!x.crange(0)+1.1*(!x.crange(1)-!x.crange(0)) 
      ylabel=!y.crange(0)+1.1*(!y.crange(1)-!y.crange(0)) 
      IF string(value(1,no_val)) ne -9999999.0 THEN BEGIN
        xyouts,xlabel,ylabel,string(value(1,no_val),format='(f9.1)'),charsize=1.2,charthick=1,color=4,/noclip,alignment=1. 
      ENDIF  
    ENDIF ELSE BEGIN    
      plot, [start_time, end_time],[0,1],yrange=[0,1],Ystyle=1 , /nodata,/YNozero, yticks=3,$
        Title=title, XTitle='Time',/box, Max_level=2, DT_Range=[start_time.julian,end_time.julian]  
    ENDELSE   

    filename=Output_dir+STRTRIM(STRING(I)+".gif",1) 
 
    IMAGE_GRAB,filename,/overwrite,/PNG  

END
