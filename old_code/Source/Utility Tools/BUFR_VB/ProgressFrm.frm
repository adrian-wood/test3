VERSION 5.00
Begin VB.Form frmProgress 
   ClientHeight    =   5970
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   5025
   LinkTopic       =   "Form1"
   ScaleHeight     =   5970
   ScaleWidth      =   5025
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdExit 
      Caption         =   "EXIT"
      Height          =   540
      Left            =   1785
      TabIndex        =   6
      Top             =   3360
      Width           =   1275
   End
   Begin VB.TextBox txtEnded 
      Height          =   330
      Left            =   2835
      TabIndex        =   5
      Top             =   2205
      Width           =   1275
   End
   Begin VB.TextBox txtRowNumber 
      Height          =   330
      Left            =   2835
      TabIndex        =   4
      Top             =   1365
      Width           =   1275
   End
   Begin VB.TextBox txtTableNumber 
      Height          =   330
      Left            =   2835
      TabIndex        =   3
      Top             =   420
      Width           =   1275
   End
   Begin VB.Label lblClosingDown 
      Height          =   495
      Left            =   1320
      TabIndex        =   7
      Top             =   4800
      Width           =   2295
   End
   Begin VB.Label lblEnded 
      Alignment       =   2  'Center
      Caption         =   "COMPLETED"
      Height          =   330
      Left            =   630
      TabIndex        =   2
      Top             =   2100
      Width           =   1170
   End
   Begin VB.Label lblRowNumber 
      Alignment       =   2  'Center
      Caption         =   "TABLE ROW"
      Height          =   330
      Left            =   735
      TabIndex        =   1
      Top             =   1260
      Width           =   1170
   End
   Begin VB.Label lblTableNumber 
      Alignment       =   2  'Center
      Caption         =   "TABLE NUMBER"
      Height          =   330
      Left            =   420
      TabIndex        =   0
      Top             =   315
      Width           =   1590
   End
End
Attribute VB_Name = "frmProgress"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdExit_Click()

   'Exit action implemented -> calls Close_All to close all files and then unloads forms.

   cmdExit.Enabled = False
   lblClosingDown.Caption = "...CLOSING DOWN..."

   Close_All
   DoEvents
   Unload frmBUFRMetadataExtraction
   Unload Me
   End
End Sub


