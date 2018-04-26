#!/usr/bin/perl
#
#
# ----------------------------------------------------------------------------------------------------------------------------------------
# PROGRAM - F2HTML_NEWA
#
# Author    Richard Weedon
#
# Date 11th Nov 2011
#
# Outline - Will construct HTML listings in which calls to subroutines / functions are linked.
#
# The program is executed in two phases, the first will scroll through the .f90 listing of each routine and identify the
# subroutines / functions called. These are identified from the "USE" statement at the start of each program.
# The information extracted is placed in two Hashes -
#
#    $calling_function (HASH)   Key - Program identifier (mdb.f90) Value => Html File details (mdb_f90.html)
#
#    $called_functions ( HASH of arrays) key - Program identifier (mdb.f90) Value(s) => called routine names (iceret,inquire,etc)
#
# The second phase will scroll through each routine listed in $calling_function and look for subroutine / Function calls.
# Details of the subroutine / function calls for each routine are supplied by the values given for that key in
# $called_functions.
#
#
#=========================================================================================================================================
# Hash declarations
%calling_function = ("dummy" => "dummy");

%called_functions = (dummy => ["dummy"]);


open(DATE, "date |");  $current_date = <DATE>;  close(DATE);
$date_message = 'Output produced by Mr W' . $perl_progname . ' at ' . $current_date;

for ($pass=1 ; $pass <= 2 ; $pass++) {
  @SAVE_ARGV = @ARGV;
# ======================================== pass1 populate HASH ===========================================================================
   if ($pass == 1) {
      foreach $input_filename (@SAVE_ARGV) {
       $file_description =  `ls -l $input_filename`;
       open(INPUT_FILE, "$input_filename");
      # print "ip file  $input_filename\n";                                                    							
         if ($test=(index($input_filename,"90") >= 0)) {                                    # establish program name & populate primary
	         $short_filename=substr($input_filename,0,($test-5));	   	            # hash
	         $short_filename =~ tr/A-Z/a-z/;
		 $full_link=$short_filename."_f90.html";
		      	
	      if ( exists($calling_function{$input_filename}) ) {                           # pass = 1 primary array to be populated
	                                                                                    # with names
		 print "Warning duplicate of $input_filename in calling function hash\n";
		} else {
		 $calling_function{$input_filename}="$full_link";
		 }
	     if ( exists($called_functions{$input_filename}) ) {
		  print "Warning duplicate of $input_filename in called function hash\n";
		} else {
		 $called_functions{$input_filename}=["NULL"];                              # NULL signifys no called routines at
		                                                                           # this point
		 }		      	
        }else {	
	  print "warning error in filename substr\n";	
        }
	
		    	
        while (<INPUT_FILE>) { 	
	s/^\s+//;                                                                           # create & populate secodary Hash array   	
	  if ( m/^\*|^!/i ) {
          } else {
	       $uset=$_;
	       $uset =~ tr/a-z/A-Z/;
	       $uset =~ s/^\s+//;
               $uset =~ s/\s+$//;
	       $test2 = index($uset,"USE ");
	
             if ($test2 >=0 && $test2 <= 6 ) {
	     #    print "uset values $uset\n";  	
	         @chars=split,$uset;
	         @chars=split,$_;
	         $chars[1] =~ tr/A-Z/a-z/; 	      	
	         $test3 = index($chars[1],"_mod");	
		 $called_filename=substr($chars[1],0,$test3);
		   if ( exists($called_functions{$input_filename}) ) {
		       push  @{ $called_functions{$input_filename} }, $called_filename;
		  } else {
		       $called_functions{$input_filename}[0] = $called_filename;
	    }
          }
	 }
        } # while
	close (INPUT_FILE);          	 		
      } # for each
  # ======================================== pass2 construct output ===========================================================================

    } elsif ($pass == 2) {
      foreach $prog (keys %calling_function) {
          $num=0;
          $filename=$calling_function{$prog};
	  $titleno=index($filename,"_");
	  $title=substr($filename,0,$titleno);
	  $title =~ tr/a-z/A-Z/;
	  chomp $prog;
	  chomp $filename;	
          open OUTPUT_FILE, "+> $filename" or die "failure in opening o/p file $filename";
          open INPUT_FILE_TWO, "$prog" or die "error opening i/p file $prog";
	 #  print "--------------\n";
	 #  print "$prog opened\n";
	    print OUTPUT_FILE "<html><title>$prog</title>\n";
	    print OUTPUT_FILE "<h3>$title  <i>( $prog  )</i></h3><hr><pre> $file_description\n";
	    print OUTPUT_FILE "\n";
	    print OUTPUT_FILE "<hr>\n";
	    print OUTPUT_FILE "\n";	    	
	    @temp_array = @{ $called_functions{$prog} };                                   # no of elements in the temp array
	    $num = @temp_array;
	    if ($num > 1) {
	      $num=$num-1;
	    }
	    $line=0;                                                                         	
	   while (<INPUT_FILE_TWO>) {
	    chomp;
	    $foundnum= -1;                                                                 # spool through the .f90 file
	    $line=$line+1;
	    $switch=0;	
	      if ($line < 10 ) {
	        $space="0000".$line;
	      } elsif ($line <= 99 && $line >= 10 ) {
	        $space="000".$line;
	      } elsif ($line <= 999 && $line >= 100 ) {
	        $space="00".$line;
	      } elsif ($line <= 9999 && $line >= 1000 ) {
	        $space="0".$line;
	      }
	    $linenum=$space.":";
	    $exc="!";   	
	    $test_com=(index($_,$exc));	
	                                                                                   # check for comment indicator	
	    if ($test_com > 0 ) {                                                          # search will be conducted up to the                                                                                             	
	       $temp_line=substr($_,0,$test_com);                                          # comment line only.	
	       $temp_line2=substr($_,$test_com);
	       $temp_line =~ tr/A-Z/a-z/;
            } elsif ($test_com == -1)  {         # returns -1 no comments present
	       $temp_line=$_;                                                               	
	       $temp_line2="";
	       $temp_line =~tr/A-Z/a-z/;
	    } else {                           #  returns 0 full comment line
	       $temp_line3 = $_;
	       $temp_line = "";
	    } 	
	       	    	
	     if ($num >= 1 ) {
	  #    print "num equals $num\n";                                                                                                                   	    										                                                                                                                                                                   	              								            											
	       for ($n=0 ;$n<=$num;$n++) {
		 $found = $temp_array[$n];
		 $testa=index($temp_line,$found);
		 $lentest=length($found);   	
		   if ($testa > -1) { 	
		     $testb=index($temp_line,"use ");
		     $testc=index($temp_line,"call");
		     $testd=index($temp_line,"(");
		       if ($testb >= 0) {
	                 $link=$found."_f90.html"; 		
		         $call_string="<a href=\"".$link."\"><b>".$found."</a></b>";
	                 $temp_line =~ s/$found/$call_string/;
		         $op_line=$linenum."  ".$temp_line.$temp_line2;
		         print OUTPUT_FILE "$op_line\n";
		         $switch=1;
			#print "USE found in $prog \n";
			#print "$temp_line \n";
			#print "looking for $found \n";
			# print "----------\n";
			
		       } elsif ($testc >= 0) {
	                 $link=$found."_f90.html"; 		
		         $call_string="<a href=\"".$link."\"><b>".$found."</a></b>";
	                 $temp_line =~ s/$found/$call_string/;
		         $op_line=$linenum."  ".$temp_line.$temp_line2;
		         print OUTPUT_FILE "$op_line\n";
		         $switch=1;
		       } else {
		         if ($testc >= 0) {
			   if ($testd > ($lentest + $found)) {
	                     $link=$found."_f90.html"; 		
		             $call_string="<a href=\"".$link."\"><b>".$found."</a></b>";
	                     $temp_line =~ s/$found/$call_string/;
		             $op_line=$linenum."  ".$temp_line.$temp_line2;
		             print OUTPUT_FILE "$op_line\n";
		             $switch=1;
			   }  		
		         }
		       }
		      }
	       } # end for
	        if ($switch < 1) {
		  if ($test_com > 0) {
		     $op_line=$linenum."  ".$temp_line.$temp_line2;
	             print OUTPUT_FILE "$op_line\n";
                  } elsif ($test_com < 0) {
		    $op_line=$linenum."  ".$temp_line.$temp_line2;
		    print OUTPUT_FILE "$op_line\n";
		  } elsif ($test_com == 0) {
		    $op_line=$linenum."  ".$temp_line3;
		    print OUTPUT_FILE "$op_line\n";
		  }
		 }
	      } else {
	         if ($test_com > 0) {
		     $op_line=$linenum."  ".$temp_line.$temp_line2;
	             print OUTPUT_FILE "$op_line\n";
                  } elsif ($test_com < 0) {
		     $op_line=$linenum."  ".$temp_line.$temp_line2;
		     print OUTPUT_FILE "$op_line\n";
		  } elsif ($test_com == 0) {
		     $op_line=$linenum."  ".$temp_line3;
		     print OUTPUT_FILE "$op_line\n";
		  } 		
	       }   	  		

           }  # while
      print OUTPUT_FILE "</pre><hr>Output produced by Mr W\n";
      print OUTPUT_FILE "<hr></html>\n";
      close (INPUT_FILE_TWO);
    close (OUTPUT_FILE);
    } # end foreach                                                                                                                                              										       										       										       										       										    										
   } # end if
  } # end for		
# ========================================================================================================================================
# establish the index page and link the main programs through it
# this process will utilise the hash and hash array created earlier.
#
#=========================================================================================================================================
# construct and populate calling hash
 foreach $item (keys %calling_function) {
   $lookfor="f90";
  # print "\n";
 #  print "==================================\n";
 #  print "calling function key $item\n";
 #  print "==================================\n";
   $itemin=index($item,$lookfor);

   $itemin=$itemin-1;
   $item2=substr($item,0,$itemin);

    foreach $aitem (keys %called_functions) {

      if ($aitem eq $item) {
     #print "matched item $aitem with $item no further action taken\n";
      } else {
     @temp_array = @{ $called_functions{$aitem} };
     $num=scalar(@temp_array);

       for ($n=0; $n<=$num; $n++) {
        $search_item = $temp_array[$n];
	$search_item =~ tr/A-Z/a-z/;

          if ($item2 eq $search_item ) {
	     if ( exists($family_functions{$item2}) ) {
	               $item2 =~ tr/A-Z/a-z/;
		       push @{ $family_functions{$item2} }, $aitem;
		  } else {
		       $item2 =~ tr/A-Z/a-z/;
		       $family_functions{$item2} = [$aitem];
	    }
	  }
	 }
	}
	
      }

    }

       	
# family functions will contain key => subroutine / function value => routines which call the subroutine / function

$filename = "Index_source_code.html";
# open index .html file
open OUTPUT_FILE, "+> $filename" or die "failure in opening o/p file $filename";

print OUTPUT_FILE "<html><head>\n";
print OUTPUT_FILE "<meta http-equiv=\"content-type\" content=\"text/html; charset=ISO-8859-1\"><title>Index File</title>\n";
print OUTPUT_FILE "</li></ul>\n";
print OUTPUT_FILE "<a name=\"alphabetic_index\"></a><hr>\n";
print OUTPUT_FILE "<pre><h3>Alphabetical list of FUNCTIONs and SUBROUTINEs and their Parents  </h3>\n";
print OUTPUT_FILE "<hr>\n";

 foreach  $x (sort keys %family_functions) {
         @sort_array = @{ $family_functions{$x} };
	 $nump=scalar(@sort_array);
	  $xlink=$x."_f90.html";
	 print OUTPUT_FILE "<a href=\"$xlink\">$x</a>\n";
         for ($n=0;$n<=$nump;$n++) {
	 $p=$sort_array[$n];
	 $p =~ s/.f90/_f90/;
	 $p =~ s/.F90/_f90/;
	 $plink=$p.".html";
         print OUTPUT_FILE "                <a href=\"$plink\">$p</a>\n";
	 }
      }

close (OUTPUT_FILE);	
























