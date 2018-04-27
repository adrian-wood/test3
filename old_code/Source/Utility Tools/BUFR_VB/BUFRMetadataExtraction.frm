VERSION 5.00
Begin VB.Form frmBUFRMetadataExtraction 
   Caption         =   "BUFR Data Extraction"
   ClientHeight    =   5655
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   5655
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdExit 
      Caption         =   "EXIT"
      Height          =   750
      Left            =   1440
      TabIndex        =   2
      Top             =   3045
      Width           =   2535
   End
   Begin VB.CommandButton cmdExtractD 
      Caption         =   "EXTRACT DATA FROM BUFR FILE D"
      Height          =   960
      Left            =   1365
      TabIndex        =   1
      Top             =   1575
      Width           =   2520
   End
   Begin VB.CommandButton cmdExtractB 
      Caption         =   "EXTRACT DATA FROM BUFR FILE B"
      Height          =   915
      Left            =   1365
      TabIndex        =   0
      Top             =   420
      Width           =   2475
   End
   Begin VB.Label lblClosingDown 
      Height          =   495
      Left            =   1440
      TabIndex        =   3
      Top             =   4560
      Width           =   1215
   End
End
Attribute VB_Name = "frmBUFRMetadataExtraction"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text


Public Sub cmdExit_Click()
 
   cmdExit.Enabled = False
   lblClosingDown.Caption = "...CLOSING DOWN..."
   Call Close_All
   Unload Me
   End
  
End Sub

Private Sub cmdExtractB_Click()

'Extracts data from BUFR file B and writes output to text file.

Dim FullDocumentRange As Word.Range
Dim current_directory, bufr_reference_filename As String
Dim file_descriptor, wmo_bufr_table_b_file, local_bufr_table_b_file, composite_bufr_table_b_file As String
Dim outString, xtrString, holdArr(1 To 11), numTestString1, numTestString2, numTestString3 As String
Dim asciitest, postString, refLineString As String
Dim tableno, colno, high_col, holdArrCount, rowno, restart_rowno, count, xValStor, yValStor, var, pos As Integer
Dim local_xval As Integer
Dim firstDataRow As Boolean
Dim progressTrackfrm As frmProgress

On Error GoTo ErrorHandlerB

'Disable and hide initial activation form.

cmdExtractB.Enabled = False
cmdExtractD.Enabled = False
frmBUFRMetadataExtraction.Hide

'initialise booleans to control file opening & closing

word_application_open = False
wmo_bufr_table_b_fileOpen = False
composite_bufr_table_b_fileOpen = False
local_bufr_table_b_fileOpen = False

   
'Check that text file bufr_ref_file.txt , containing all principal datafile locations exists
'in same directory as executable.   Then open file directory_containing_executable\bufr_ref_file.txt
   
current_directory = CurDir
bufr_reference_filename = current_directory & "\bufr_ref_file.txt"
If Dir(bufr_reference_filename) = "" Then Error 990

Open bufr_reference_filename For Input As #9

'Initialise variables to hold principal datafile names for input and output.

wmo_bufr_table_b_file = ""
local_bufr_table_b_file = ""
composite_bufr_table_b_file = ""

'Read principal datafile names for input and output from file directory_containing_executable\bufr_ref_file.txt

Do
  Line Input #9, refLineString
  If Mid(refLineString, 1, 1) <> "!" Then
     If Mid(refLineString, 1, 16) = "WMO_BUFR_TABLE_B" Then
        file_descriptor = "WMO_BUFR_TABLE_B"
     ElseIf Mid(refLineString, 1, 18) = "LOCAL_BUFR_TABLE_B" Then
        file_descriptor = "LOCAL_BUFR_TABLE_B"
     ElseIf Mid(refLineString, 1, 22) = "COMPOSITE_BUFR_TABLE_B" Then
        file_descriptor = "COMPOSITE_BUFR_TABLE_B"
     ElseIf file_descriptor = "WMO_BUFR_TABLE_B" And wmo_bufr_table_b_file = "" Then
        wmo_bufr_table_b_file = Trim(refLineString)
     ElseIf file_descriptor = "LOCAL_BUFR_TABLE_B" And local_bufr_table_b_file = "" Then
        local_bufr_table_b_file = Trim(refLineString)
     ElseIf file_descriptor = "COMPOSITE_BUFR_TABLE_B" And composite_bufr_table_b_file = "" Then
        composite_bufr_table_b_file = Trim(refLineString)
     End If
   End If
Loop Until Mid(refLineString, 1, 3) = "END"

'Existence checks for input files.

If Dir(wmo_bufr_table_b_file) = "" Then Error 991
If Dir(local_bufr_table_b_file) = "" Then Error 992
   
'Create WORD application

Set wordApp = CreateObject("Word.Application") 'creates a word application object
wordApp.Visible = False          'to prevent word visibly opening a document
word_application_open = True     'Boolean to keep track of open or closed status of word application

DoEvents   'Finish what is going on before going to next line

'Open BUFR table WORD file

Word.Application.Documents.Open FileName:=wmo_bufr_table_b_file 'Open the actual word document
wmo_bufr_table_b_fileOpen = True                                'keeps track if Bufr file B is open
Set FullDocumentRange = ActiveDocument.Content                  'Gets the content of the Bufr B document
If FullDocumentRange.Find.Found = True Then var = MsgBox("FOUND", vbOKOnly)  'Check to make sure document

' Open text file for output
   
Open composite_bufr_table_b_file For Output As #1   'Output file
composite_bufr_table_b_fileOpen = True              'keeps track of output file

' Open local BUFR text file

Open local_bufr_table_b_file For Input As #2     'Open current MetDB version of Table B
local_bufr_table_b_fileOpen = True               'Keeps track of MetDB current version
localX = 0                                       'Keeps track of current xxx value in MetDB current version
mainX = 0                                        'Keeps track of xxx value in WMO word document

' Read header string from local BUFR text file and write it to output file.

Line Input #2, refLineString
Print #1, refLineString
   
' Set up progress-tracking frame.
' As the BUFR WORD file is being processes , this frame will indicate table and row being processed.
   
Set progressTrackfrm = New frmProgress 'This creates the form which shows the progress of the extraction
progressTrackfrm.Caption = "EXTRACTING DATA FROM BUFR FILE B"  'sets up caption for the progress tracking form
progressTrackfrm.Visible = True     'Opens form so visible to user
progressTrackfrm.txtEnded.Text = "NO" 'Sets initial value of Completed field called txtEnded as a field variable
      
      
' Initialise value of xValStor.
' xValStor stores the value of the previous X variable read from the BUFR word file whilst the new value
' read from the BUFR word file and is being processed.
      
xValStor = -1

' Initialise value of local_xval.
' local_xVal stores the last X value read from the local BUFR text file by Function GetLocalDataB.


local_xval = -1

'Loop through tables.
   
For tableno = 2 To ActiveDocument.Tables.count

   'Omit tables which are in the notes and do not carry valid metadata.

   If tableno <> 13 And tableno <> 25 And tableno <> 28 And tableno <> 32 Then
   
      'Setup form to exhibit progress.
   
      progressTrackfrm.txtTableNumber.Text = Str(tableno) & " \ " & Str(ActiveDocument.Tables.count) 'table number on progress form
   
      firstDataRow = True
   
      'Loop through rows in table.
   
      For rowno = 1 To ActiveDocument.Tables(tableno).Rows.count  'bit at end gets number of rows in the current table
      
         'Reset array elements of holdArr to empty strings
      
         For holdArrCount = 1 To UBound(holdArr)
             holdArr(holdArrCount) = ""
         Next holdArrCount
      
   
      
         'Extraction numeric characters from column 3 of row.
         'Different versions of word require this to be different, the code here copes with
         'Word 97 and Word 2003. If you checked for numeric values from column 1 and 2 with Word 2003
         'then the application would fall over due to not be existent in certain header columns.
   
         xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 3).Range.Text) 'extract text in col 3
         numTestString3 = ""   'Blank string into which numeric characters are loaded in
         For pos = 1 To Len(xtrString) 'Loop over each character in the extracted string
            asciitest = Mid(xtrString, pos, 1)  'extract each character into asciitest
            If Asc(asciitest) >= 48 And Asc(asciitest) <= 57 Then 'gets ASCII numerical value and checks
                                                                  'if in expected range
               numTestString3 = numTestString3 & asciitest        'If ok then put into numTestString3
            End If
         Next pos
         
         'If a 3 digit number has been extracted from column 3 of the row then....
         
         If IsNumeric(numTestString3) And Len(numTestString3) = 3 And _
         rowno <= ActiveDocument.Tables(tableno).Rows.count Then
         
            'Extract the 2 digit number from column 1 of the row.
         
            xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 1).Range.Text)
            numTestString1 = ""
            For pos = 1 To Len(xtrString)
               asciitest = Mid(xtrString, pos, 1)
               If Asc(asciitest) >= 48 And Asc(asciitest) <= 57 Then
                  numTestString1 = numTestString1 & asciitest
               End If
            Next pos
            
            'Extract the 2 digit number from column 2 of the row.
   
            xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 2).Range.Text)
            numTestString2 = ""
            For pos = 1 To Len(xtrString)
               asciitest = Mid(xtrString, pos, 1)
               If Asc(asciitest) >= 48 And Asc(asciitest) <= 57 Then
                  numTestString2 = numTestString2 & asciitest
               End If
            Next pos
      
            'If first data row with that X value then extract all the local file data with Y>191 for the
            'previous X value. i.e. you have read in all the global descriptors for the class so now
            ' read in all the local descriptors for the class from the current MetDB Table B.
      
            If firstDataRow Then
               If Val(numTestString2) <> xValStor Then
                  mainX = xValStor
                  GetLocalDataB
                  xValStor = Val(Trim(numTestString2))
                  firstDataRow = False
               End If
            End If
            
            'If Bufr table row is to processed then...................
   
            If Val(numTestString3) < 192 Then
               holdArr(1) = Trim(numTestString1)
               holdArr(2) = Trim(numTestString2)
               holdArr(3) = Trim(numTestString3)
               
               
               'Increment row number on progress form.
      
               progressTrackfrm.txtRowNumber.Text = Str(rowno)
               DoEvents
     
         
               'Loop through remaining columns in that row and extract the rest of the metadata for that row.

               If ActiveDocument.Tables(tableno).Columns.count >= 4 Then high_col = 4 'check number of cols.
               If ActiveDocument.Tables(tableno).Columns.count >= 5 Then high_col = 5

               For colno = 4 To high_col
                  xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, colno).Range.Text)
                  For pos = 1 To Len(xtrString)
                     asciitest = Mid(xtrString, pos, 1)
                     If Asc(asciitest) = 150 Then asciitest = Chr(45) 'this check put in because of negative sign
                     If Asc(asciitest) >= 32 And Asc(asciitest) <= 126 Then 'ascii in normal printing range
                        holdArr(colno) = holdArr(colno) & asciitest
                     End If
                  Next pos
               Next colno
               
               If LCase(Trim(holdArr(4))) <> "reserved" Then 'Some col 4 have reserved and rest of row to be ignored
               
                  If ActiveDocument.Tables(tableno).Columns.count >= 6 Then high_col = 6
                  If ActiveDocument.Tables(tableno).Columns.count >= 7 Then high_col = 7
                  If ActiveDocument.Tables(tableno).Columns.count >= 8 Then high_col = 8 'col checks
               
                  For colno = 6 To high_col
                     xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, colno).Range.Text)
                     For pos = 1 To Len(xtrString)
                        asciitest = Mid(xtrString, pos, 1)
                        If Asc(asciitest) = 30 Then asciitest = Chr(45) 'another minus character to reset
                        If Asc(asciitest) = 150 Then asciitest = Chr(45) 'another minus to reset
                        If (Asc(asciitest) >= 48 And Asc(asciitest) <= 57) Or (Asc(asciitest) = 45) Then 'if numeric or minus
                           holdArr(colno) = holdArr(colno) & asciitest
                        End If
                     Next pos
                  Next colno
                  
                  If ActiveDocument.Tables(tableno).Columns.count >= 9 Then high_col = 9
               
                  For colno = 9 To high_col
                     xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, colno).Range.Text)
                     For pos = 1 To Len(xtrString)
                        asciitest = Mid(xtrString, pos, 1)
                        If Asc(asciitest) = 150 Then asciitest = Chr(45)
                        If Asc(asciitest) >= 32 And Asc(asciitest) <= 126 Then
                           holdArr(colno) = holdArr(colno) & asciitest
                        End If
                     Next pos
                  Next colno
                  
                  If ActiveDocument.Tables(tableno).Columns.count >= 10 Then high_col = 10
                  If ActiveDocument.Tables(tableno).Columns.count >= 11 Then high_col = 11
               
                  For colno = 10 To high_col
                     xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, colno).Range.Text)
                     For pos = 1 To Len(xtrString)
                        asciitest = Mid(xtrString, pos, 1)
                        If Asc(asciitest) = 30 Then asciitest = Chr(45)
                        If Asc(asciitest) = 150 Then asciitest = Chr(45)
                        If (Asc(asciitest) >= 48 And Asc(asciitest) <= 57) Or (Asc(asciitest) = 45) Then
                            holdArr(colno) = holdArr(colno) & asciitest
                        End If
                     Next pos
                  Next colno
               
                  'Print the metadata from first 4 columns to the output file
         
                  Print #1, Trim(holdArr(1)) & Trim(holdArr(2)) & Trim(holdArr(3)) & " " & _
                  UCase(Mid(Trim(holdArr(4)), 1, 60))
               
                  'Build up value of String outString from the values extracted from columns 5 upwards.
           
                  outString = " "
         
                  postString = String(24, " ")
                  LSet postString = Trim(Mid(holdArr(5), 1, 24))
                  outString = outString & postString
         
                  postString = String(4, " ")
                  RSet postString = Trim(Mid(holdArr(6), 1, 4))
                  outString = outString & postString

                  postString = String(11, " ")
                  RSet postString = Trim(Mid(holdArr(7), 1, 11))
                  outString = outString & postString
         
                  postString = String(3, " ")
                  RSet postString = Trim(Mid(holdArr(8), 1, 3))
                  outString = outString & postString
         
                  postString = String(24, " ")
                  LSet postString = Trim(Mid(holdArr(9), 1, 24))
                  outString = outString & " " & postString
         
                  postString = String(4, " ")
                  RSet postString = Trim(Mid(holdArr(10), 1, 4))
                  outString = outString & postString
         
                  postString = String(3, " ")
                  RSet postString = Trim(Mid(holdArr(11), 1, 3))
                  outString = outString & postString
               
                  'Print metadata to output file.
        
                  outString = UCase(outString)
                  Print #1, outString
                  
               End If
         
            End If
         
         End If
         
         DoEvents
         
      Next rowno
 
   End If
 
Next tableno

'Last data row in file has been read.
'Extract all the local file data with Y>191 for the current X value.

mainX = 999

GetLocalDataB

progressTrackfrm.txtEnded.Text = "YES"

DoEvents

Exit Sub


ErrorHandlerB:

If Err.Number = 990 Then
   var = MsgBox("Primary reference file : bufr_ref_file.txt NOT FOUND", vbOKOnly)
ElseIf Err.Number = 991 Then
   var = MsgBox("WMO BUFR FILE B NOT FOUND", vbOKOnly)
ElseIf Err.Number = 992 Then
   var = MsgBox("LOCAL_BUFR_TABLE_B NOT FOUND", vbOKOnly)
Else
   var = MsgBox("FATAL ERROR No." & Err.Number & " : " & Err.Description, vbOKOnly)
End If
Call Close_All
End

DoEvents
   
End Sub

Private Sub cmdExtractD_Click()

'Extracts data from BUFR file D and writes output to text file.

Dim theDocument As Word.Document                                             'WMO word document
Dim FullDocumentRange As Word.Range                                          'context variable for inside of document
Dim progressTrackfrm As frmProgress                                          'progress tracker for the form
Dim tableno, colno, rowno, restart_rowno, count, local_xval, var As Integer
Dim holdArrCount, outputCount, pos As Integer
Dim file_descriptor, current_directory, bufr_reference_filename, wmo_bufr_table_d_file As String
Dim local_bufr_table_d_file, composite_bufr_table_d_file As String           'current and merged BUFR D tables
Dim asciitest, outString, xtrString, holdArrCountString As String
Dim holdCol4, holdCol5, holdCol6, numTestString1, numTestString2, numTestString3 As String
Dim fStor, xStor, yStor, currentOutputHeader, nextOutputHeader, refLineString, outputString As String
Dim holdArr(100) As String  'This is a limitation of 100 which seems to be enough but may require a increase                                                  '

On Error GoTo ErrorHandlerD

'Disable and hide initial activation form.

cmdExtractB.Enabled = False
cmdExtractD.Enabled = False
frmBUFRMetadataExtraction.Hide

'initialise booleans to control file opening & closing

word_application_open = False
wmo_bufr_table_d_fileOpen = False
composite_bufr_table_d_fileOpen = False
local_bufr_table_d_fileOpen = False

'Check that text file bufr_ref_file.txt , containing all principal datafile locations exists
'in same directory as executable.   Then open file directory_containing_executable\bufr_ref_file.txt

current_directory = CurDir
bufr_reference_filename = current_directory & "\bufr_ref_file.txt"
If Dir(bufr_reference_filename) = "" Then Error 990

Open bufr_reference_filename For Input As #9

'Initialise variables to hold principal datafile names for input and output.

wmo_bufr_table_d_file = ""
local_bufr_table_d_file = ""
composite_bufr_table_d_file = ""

'Read principal datafile names for input and output from file directory_containing_executable\bufr_ref_file.txt

Do
  Line Input #9, refLineString
  If Mid(refLineString, 1, 1) <> "!" Then
     If Mid(refLineString, 1, 16) = "WMO_BUFR_TABLE_D" Then
        file_descriptor = "WMO_BUFR_TABLE_D"
     ElseIf Mid(refLineString, 1, 18) = "LOCAL_BUFR_TABLE_D" Then
        file_descriptor = "LOCAL_BUFR_TABLE_D"
     ElseIf Mid(refLineString, 1, 22) = "COMPOSITE_BUFR_TABLE_D" Then
        file_descriptor = "COMPOSITE_BUFR_TABLE_D"
     ElseIf file_descriptor = "WMO_BUFR_TABLE_D" And wmo_bufr_table_d_file = "" Then
        wmo_bufr_table_d_file = Trim(refLineString)
     ElseIf file_descriptor = "LOCAL_BUFR_TABLE_D" And local_bufr_table_d_file = "" Then
        local_bufr_table_d_file = Trim(refLineString)
     ElseIf file_descriptor = "COMPOSITE_BUFR_TABLE_D" And composite_bufr_table_d_file = "" Then
        composite_bufr_table_d_file = Trim(refLineString)
     End If
   End If
Loop Until Mid(refLineString, 1, 3) = "END"

'Existence checks for input files. Throws error if they don't exist

If Dir(wmo_bufr_table_d_file) = "" Then Error 991
If Dir(local_bufr_table_d_file) = "" Then Error 992

'Open BUFR table WORD file

Set wordApp = CreateObject("Word.Application")  'creates application object
wordApp.Visible = False
word_application_open = True

Word.Application.Documents.Open FileName:=wmo_bufr_table_d_file 'associates application object with a word document
wmo_bufr_table_d_fileOpen = True                                'Boolean to say word document is open

'Make sure content can be accessed
Set FullDocumentRange = ActiveDocument.Content
If FullDocumentRange.Find.Found = True Then var = MsgBox("FOUND", vbOKOnly) 'Not throwing an error

'Set Boolean file control variables
   
Open composite_bufr_table_d_file For Output As #3  'This will be new table D file
composite_bufr_table_d_fileOpen = True
Open local_bufr_table_d_file For Input As #4  'Metdb current BUFR table D file
local_bufr_table_d_fileOpen = True
localX = 0
mainX = 0

' Read header string from current BUFR table text file and write it to output file.
' This may need amendment if the BUFR WORD file is a new version.

Line Input #4, refLineString
Print #3, refLineString

' Set up progress-tracking frame.
' As the BUFR WORD file is being processes , this frame will indicate table and row being processed.
   
Set progressTrackfrm = New frmProgress
progressTrackfrm.Caption = "EXTRACTING DATA FROM BUFR FILE D"
progressTrackfrm.Visible = True
progressTrackfrm.txtEnded.Text = "NO"

'Initialise metadata storage variables for metadata extraction loop

fStor = ""
xStor = ""
yStor = ""


'Keeps track of output headers
currentOutputHeader = ""
nextOutputHeader = ""

'Keeps track of were you are in the holdArr array which holds the descriptors
holdArrCount = 0

'For each table...

For tableno = 2 To ActiveDocument.Tables.count  'Loop over the tables, table 1 missed out as not required.

   'Set current table number in progress form.

   progressTrackfrm.txtTableNumber.Text = Str(tableno) & " \ " & Str(ActiveDocument.Tables.count)
   
   'For each row in table...
   
   For rowno = 1 To ActiveDocument.Tables(tableno).Rows.count
      
      'Set current row number in progress form.
      
      progressTrackfrm.txtRowNumber.Text = Str(rowno)
      DoEvents
     
      'Extract alphanumeric characters from first 3 cells of row.
      
      xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 1).Range.Text)
      numTestString1 = ""
      For pos = 1 To Len(xtrString)
         asciitest = Mid(xtrString, pos, 1)
         If Asc(asciitest) >= 33 And Asc(asciitest) <= 126 Then
            numTestString1 = numTestString1 & asciitest
         End If
      Next pos
   
      xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 2).Range.Text)
      numTestString2 = ""
      For pos = 1 To Len(xtrString)
         asciitest = Mid(xtrString, pos, 1)
         If Asc(asciitest) >= 33 And Asc(asciitest) <= 126 Then
            numTestString2 = numTestString2 & asciitest
         End If
      Next pos
   
      xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 3).Range.Text)
      numTestString3 = ""
      For pos = 1 To Len(xtrString)
         asciitest = Mid(xtrString, pos, 1)
         If Asc(asciitest) >= 33 And Asc(asciitest) <= 126 Then
            numTestString3 = numTestString3 & asciitest
         End If
      Next pos
      
      'Cell 3 occasionally has a textual flag appended to the number referring to notes below.
      'This flag character needs to be stripped of the number.
      
      If Len(numTestString3) > 3 Then
         If IsNumeric(Mid(numTestString3, 1, 3)) Then
            numTestString3 = Mid(numTestString3, 1, 3)
         End If
      End If
      
     
      'If cell 1 contains a 1-digit number & cell 2 contains a 2-digit number & cell 3 contains a 3-digit number
      
      If IsNumeric(numTestString1) And IsNumeric(numTestString2) And IsNumeric(numTestString3) And _
      Len(numTestString1) = 1 And Len(numTestString2) = 2 And Len(numTestString3) = 3 Then
      
         'If there has been a change in values of f,x,y since previous processed row then................
      
         If Not StrComp(numTestString1, fStor, 1) Or Not StrComp(numTestString2, xStor, 1) Or Not StrComp(numTestString3, yStor, 1) Then
            
            ' print out stored header from previous row column 7, if exists.
            
            If Len(currentOutputHeader) > 0 Then
               currentOutputHeader = UCase(Mid(currentOutputHeader, 1, 70))
               Print #3, currentOutputHeader
            End If
            
            ' print output for that fxy combination as :
            ' fxxyyy
            'no.of 6-digit metadata numbers to follow
            '6-digit metadata number set at 10 numbers per line
            
            outputString = fStor & xStor & yStor
            
            If IsNumeric(outputString) Then
               If holdArrCount < 10 Then
                  holdArrCountString = " " & Str(holdArrCount)
               Else
                  holdArrCountString = Str(holdArrCount)
               End If
               outputString = outputString & holdArrCountString & " "
            End If
            
            'print out in blocks of 10
            For outputCount = 1 To holdArrCount
               outputString = outputString & holdArr(outputCount) & " "
               If outputCount Mod 10 = 0 Then
                  Print #3, outputString
                  outputString = ""
               End If
            Next outputCount
               
            If outputCount Mod 10 <> 1 Then
               Print #3, outputString
            End If
            
            Print #3, " "
            
            'If there has been a change in value of f or x since previous processed row then...........
            
            If Val(numTestString1) <> Val(fStor) Or Val(numTestString2) <> Val(xStor) Then
            
               ' print out data for y>192......copy lines direct from local file.
               GetLocalDataD                  'get local sequences from Current MetDB version
               fStor = Trim(numTestString1)
               xStor = Trim(numTestString2)
               yStor = Trim(numTestString3)
               mainX = Val(xStor)             'position in main word document
            
            End If
         
         End If
   
         'if y value < 192 then........
         '< 192 means global value
   
         If Val(numTestString3) < 192 Then  'load holdArr array with descriptors
         
            'Store current values of f,x,y
         
            fStor = Trim(numTestString1)
            xStor = Trim(numTestString2)
            yStor = Trim(numTestString3)
            
            'initialise array to hold processed cell contents for cells 4 upwards of that row.
               
            For outputCount = 1 To UBound(holdArr)
               holdArr(holdArrCount) = ""
            Next outputCount
      
            holdArrCount = 0
                                    
            xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 4).Range.Text)
            holdCol4 = ""
            For pos = 1 To Len(xtrString)
               asciitest = Mid(xtrString, pos, 1)
               If Asc(asciitest) >= 48 And Asc(asciitest) <= 57 Then
                  holdCol4 = holdCol4 & asciitest
               End If
            Next pos
            
            holdCol4 = Mid(holdCol4, 1, 1)
         
            xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 5).Range.Text)
            holdCol5 = ""
            For pos = 1 To Len(xtrString)
               asciitest = Mid(xtrString, pos, 1)
               If Asc(asciitest) >= 48 And Asc(asciitest) <= 57 Then
                  holdCol5 = holdCol5 & asciitest
               End If
            Next pos
            
            holdCol5 = Mid(holdCol5, 1, 2)
 
            xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 6).Range.Text)
            holdCol6 = ""
            For pos = 1 To Len(xtrString)
               asciitest = Mid(xtrString, pos, 1)
               If Asc(asciitest) >= 48 And Asc(asciitest) <= 57 Then
                  holdCol6 = holdCol6 & asciitest
               End If
            Next pos
            
            holdCol6 = Mid(holdCol6, 1, 3)
            
            If IsNumeric(holdCol4) And IsNumeric(holdCol5) And IsNumeric(holdCol6) Then
               holdArrCount = holdArrCount + 1
               holdArr(holdArrCount) = holdCol4 & holdCol5 & holdCol6  'load descriptors into holdArr
            End If
            
         End If
               
      ElseIf Len(Trim(numTestString1)) = 0 And Len(Trim(numTestString2)) = 0 And _
         Len(Trim(numTestString3)) = 0 Then  'Blank in first 3 columns (ie sequence number)
         
         'Extract strings from cells 4 , 5 , 6 of row.  (Descriptors associated with sequence)
      
         xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 4).Range.Text)
         holdCol4 = ""
         For pos = 1 To Len(xtrString)
            asciitest = Mid(xtrString, pos, 1)
            If Asc(asciitest) >= 48 And Asc(asciitest) <= 57 Then
               holdCol4 = holdCol4 & asciitest
            End If
         Next pos
         
         holdCol4 = Mid(holdCol4, 1, 1)
         
         xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 5).Range.Text)
         holdCol5 = ""
         For pos = 1 To Len(xtrString)
            asciitest = Mid(xtrString, pos, 1)
            If Asc(asciitest) >= 48 And Asc(asciitest) <= 57 Then
               holdCol5 = holdCol5 & asciitest
            End If
         Next pos
         
         holdCol5 = Mid(holdCol5, 1, 2)
 
         xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 6).Range.Text)
         holdCol6 = ""
         For pos = 1 To Len(xtrString)
            asciitest = Mid(xtrString, pos, 1)
            If Asc(asciitest) >= 48 And Asc(asciitest) <= 57 Then
               holdCol6 = holdCol6 & asciitest
            End If
         Next pos
         
         holdCol6 = Mid(holdCol6, 1, 3)
         
         'If cells 4 , 5 , 6 of row (holds a descriptor) all have numeric content then load into holding array for
         'processing.
            
         If IsNumeric(holdCol4) And IsNumeric(holdCol5) And IsNumeric(holdCol6) Then
            holdArrCount = holdArrCount + 1
            holdArr(holdArrCount) = holdCol4 & holdCol5 & holdCol6

         
         Else
           'If any of cells 4 , 5 , 6 have non-numeric content then process new output header from column 7.
           ' i.e. no descriptor on line
               
            currentOutputHeader = nextOutputHeader
            xtrString = Trim(ActiveDocument.Tables(tableno).Cell(rowno, 7).Range.Text)
            nextOutputHeader = ""
            For pos = 1 To Len(xtrString)
               asciitest = Mid(xtrString, pos, 1)
               If Asc(asciitest) >= 32 And Asc(asciitest) <= 126 Then
                  nextOutputHeader = nextOutputHeader & asciitest
               End If
            Next pos
            nextOutputHeader = Trim(nextOutputHeader)
            If Len(nextOutputHeader) > 0 Then
              nextOutputHeader = String(10, " ") & nextOutputHeader
            End If
            
         End If
               
      End If
      
   Next rowno
   
   DoEvents
 
Next tableno

'Output final header and data after exit from loop.

If Len(currentOutputHeader) > 0 Then
   currentOutputHeader = UCase(Mid(currentOutputHeader, 1, 70))
   Print #3, currentOutputHeader
End If
            
outputString = fStor & xStor & yStor
            
If IsNumeric(outputString) Then
   If holdArrCount < 10 Then
      holdArrCountString = " " & Str(holdArrCount)
   Else
      holdArrCountString = Str(holdArrCount)
   End If
   outputString = outputString & holdArrCountString & " "
End If
                          
For outputCount = 1 To holdArrCount
   outputString = outputString & holdArr(outputCount) & " "
   If outputCount Mod 10 = 0 Then
      Print #3, outputString
      outputString = ""
   End If
Next outputCount
               
If outputCount Mod 10 <> 1 Then
   Print #3, outputString
End If
            
Print #3, " "
            
' print out data for y>192......copy lines direct from local file.
mainX = 999
GetLocalDataD

DoEvents

progressTrackfrm.txtEnded.Text = "YES"

Exit Sub

ErrorHandlerD:

If Err.Number = 990 Then
   var = MsgBox("Primary reference file : bufr_ref_file.txt NOT FOUND", vbOKOnly)
ElseIf Err.Number = 991 Then
   var = MsgBox("WMO BUFR FILE D NOT FOUND", vbOKOnly)
ElseIf Err.Number = 992 Then
   var = MsgBox("LOCAL_BUFR_TABLE_D NOT FOUND", vbOKOnly)
Else
   var = MsgBox("FATAL ERROR No." & Err.Number & " : " & Err.Description, vbOKOnly)
End If

Call Close_All
DoEvents
End

End Sub


