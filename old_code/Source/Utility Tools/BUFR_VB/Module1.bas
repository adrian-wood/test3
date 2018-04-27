Attribute VB_Name = "Module1"
Option Explicit
Public wordApp As Word.Application
Public wmo_bufr_table_b_fileOpen, composite_bufr_table_b_fileOpen, local_bufr_table_b_fileOpen As Boolean
Public wmo_bufr_table_d_fileOpen, composite_bufr_table_d_fileOpen, local_bufr_table_d_fileOpen As Boolean
Public word_application_open, xtractingB, xtractingD As Boolean
Public refLineString, prevLineString As String
Public mainX, localX As Integer

Public Sub GetLocalDataB()

'Gets local data information for records with Y value >191.
'Local variable xval & argument localX keep track of the current position in the local file in terms of the X value
'of the record in the local file.

Dim refLineString As String
Dim xval, yval As Integer

xval = localX

'Extract information from the file whilst the current local record X value is less than or equal to
'the required BUFR file X value .

Do While Not EOF(2) And xval <= mainX 'do while not end of file or don't go past x value in main file
   yval = Val(Mid(refLineString, 4, 3))  'read in the yyy value
   If Not EOF(2) And yval < 192 And (xval <= 48 Or xval >= 63) Then
      Do
        Line Input #2, refLineString
      Loop Until IsNumeric(Mid(refLineString, 1, 6)) Or EOF(2)
      xval = Val(Mid(refLineString, 2, 2))
      yval = Val(Mid(refLineString, 4, 3))
   Else
      ' write current line and next line to output file
      refLineString = UCase(refLineString)
      Print #1, refLineString
      If Not EOF(2) Then Line Input #2, refLineString
      refLineString = UCase(refLineString)
      Print #1, refLineString
      ' read next line for testing x and y values
      If Not EOF(2) Then Line Input #2, refLineString
      xval = Val(Mid(refLineString, 2, 2))
   End If
   DoEvents
Loop

localX = xval

End Sub

Public Sub GetLocalDataD()


Dim xval, yval, numberOfItems, numberOfLinesToRead, linecount As Integer


'Gets local data information for records with Y value >191.
'Local variable xval & argument localX keep track of the current position in the local file in terms of the X value
'of the record in the local file.

xval = localX                                    'position in current Metdb file

'Read further down the file if necessary until the current local record X value is no longer less than
'the required BUFR file X value.

Do While xval < mainX And Not EOF(4)
   
   Do
      prevLineString = refLineString
      Line Input #4, refLineString
   Loop Until IsNumeric(Mid(refLineString, 1, 6)) Or EOF(4)
   
   xval = Val(Mid(refLineString, 2, 2))
   yval = Val(Mid(refLineString, 4, 3))
   numberOfItems = Val(Mid(refLineString, 8, 3))
   numberOfLinesToRead = ((numberOfItems + 9) \ 10) - 1
   
   If yval < 192 And (xval <= 48 Or xval >= 63) Then
      For linecount = 1 To numberOfLinesToRead
         prevLineString = refLineString
         Line Input #4, refLineString
      Next linecount
   Else
      prevLineString = UCase(prevLineString)
      Print #3, prevLineString
      refLineString = UCase(refLineString)
      Print #3, refLineString
      For linecount = 1 To numberOfLinesToRead
         prevLineString = refLineString
         Line Input #4, refLineString
         refLineString = UCase(refLineString)
         Print #3, refLineString
      Next linecount
      Print #3, ""
   End If
      
Loop

'Extract information from the file whilst the current local record X value is the same as
'the required BUFR file X value .

Do While xval = mainX And Not EOF(4)
   yval = Val(Mid(refLineString, 4, 3))
   numberOfItems = Val(Mid(refLineString, 8, 3))
   numberOfLinesToRead = ((numberOfItems + 9) \ 10) - 1
   If yval < 192 And (xval <= 48 Or xval >= 63) Then
      For linecount = 1 To numberOfLinesToRead
         prevLineString = refLineString
         Line Input #4, refLineString
      Next linecount
   Else
      ' write current line and required succeeding lines to output file
      If prevLineString <> "" Then
         prevLineString = UCase(prevLineString)
         Print #3, prevLineString
      End If
      refLineString = UCase(refLineString)
      Print #3, refLineString
      For linecount = 1 To numberOfLinesToRead
         prevLineString = refLineString
         Line Input #4, refLineString
         refLineString = UCase(refLineString)
         Print #3, refLineString
      Next linecount
      Print #3, ""
   End If
   
   Do
      prevLineString = refLineString
      Line Input #4, refLineString
   Loop Until IsNumeric(Mid(refLineString, 1, 6)) Or EOF(4)
   
   xval = Val(Mid(refLineString, 2, 2))
   
Loop

localX = xval 'reset

End Sub


Public Function Close_All()

  'Checks Boolean indicators for open files and then closes any files flagged as open.
  
  If word_application_open = True Then
     Word.Application.ActiveDocument.Close
     DoEvents
     Application.Quit
     DoEvents
     word_application_open = False
  End If
  
  If composite_bufr_table_b_fileOpen = True Then
     Close #1
     DoEvents
     composite_bufr_table_b_fileOpen = False
  End If
  
  If local_bufr_table_b_fileOpen = True Then
     Close #2
     DoEvents
     local_bufr_table_b_fileOpen = False
  End If
  
  If composite_bufr_table_d_fileOpen = True Then
     Close #3
     DoEvents
     composite_bufr_table_d_fileOpen = False
  End If
  
  If local_bufr_table_d_fileOpen = True Then
     Close #4
     DoEvents
     local_bufr_table_d_fileOpen = False
  End If
  
  DoEvents
    
End Function
