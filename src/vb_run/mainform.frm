VERSION 5.00
Begin VB.Form mainform 
   Caption         =   "Run Logicbase"
   ClientHeight    =   3165
   ClientLeft      =   6945
   ClientTop       =   1665
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   3165
   ScaleWidth      =   4680
   Begin VB.TextBox Solution 
      Height          =   2895
      Left            =   120
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   2
      Top             =   120
      Width           =   3495
   End
   Begin VB.HScrollBar HScroll1 
      Height          =   30
      Left            =   120
      TabIndex        =   1
      Top             =   1320
      Width           =   30
   End
   Begin VB.CommandButton Run 
      Caption         =   "Run"
      Height          =   495
      Left            =   3720
      TabIndex        =   0
      Top             =   120
      Width           =   855
   End
End
Attribute VB_Name = "mainform"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' KnowledgeWright Visual Basic Interface Prototype
' Copyright (c) 2000-2001 Amzi! inc. All Rights Reserved.
'
' BEFORE RUNNING THIS PROGRAM, the following files in the 'workshop' subdirectory
' must either be on the system environment PATH variable, or copied to the same
' directory as this program: amzi.dll, aosutils.lsx, aodbc.lsx, amzi.cfg
' Also you need to copy the jig reasoning engine (.xpl) file from the 'workshop/jigs'
' subdirectory (e.g. basic.xpl for the basic jig), and a knowledgebase file (.kb)
' from the samples subdirectory.
'
' This is the main function. It loads and runs a knowledgebase using the KWI.
' The KWI is a Prolog language calling interface to KnowledgeWright, so all
' the KWI calls are made through the Amzi! Logic Server.

Private Sub Run_Click()
    Dim s As String, id As String, responseFunctor As String, factName As String, userResponse As String, delimiter As String
    Dim responseType As String, jig As String, kbName As String, listFacts As String
    Dim Term As Long, solveTerm As Long, actionTerm As Long, responseTerm As Long, listTerm As Long
    Dim tf As Integer, i As Integer
    Dim slotList As New Collection, choiceList As New Collection, errorList As New Collection
    Dim more As Boolean
    Dim choice, entry
    
    ' Tell the Amzi! Logic Server to throw error 31300 if it has a problem
    ' We catch that below in DisplayError so that we can get the error
    ' details from the KWI.
    AmziLogicServer.ErrorMethod = 1
    On Error GoTo DisplayError
    
    ' Initialize the Amzi! Logic Server and load the jig runtime (.xpl file)
    ' There is no KWI session id right now. This is used in the error handler
    ' to determine whether the error comes from the Amzi! Logic Server or the
    ' KnowledgeWright KWI.
    id = "none"
    
    ' Initialize a new Logic Server
    Call InitLS("")
    
    ' Tell the Logic Server to load any LSX's that are specified in the amzi.cfg file
    ' Both aosutils and aodbc are required
    Call AddLSX("aosutils")
    Call AddLSX("aodbc")
    
    ' Get the name of the jig to run (an Amzi! Prolog .xpl binary program file)
    jig = InputBox("Jig Runtime Name (no extension):")
    LoadLS (App.path + "\" + jig + ".xpl")

    ' Initialize the runtime kwi
    ' Prior to starting a new KWI session the session id must be kw_init.
    id = "kw_init"
    ' The directory is where the kb file is located
    ' The session_directory is where a temporary file is created to maintain the state
    '   of the reasoning process
    ' The log_file is a log of the reasoning process and is created in the directory
    ' The message level is the level of detail in the log_file
    ' The debug_msgs file is for internal debugging messages
    s = "kwi(" + id + ", initialize([directory = $" + doubleSlashes(App.path) + "\\$, session_directory = $" + doubleSlashes(App.path) + "\\$, log_file = $vbrun.log$, debug_msgs = $vbdebug.log$, message_level = high]), _)"
    tf = ExecStrLS(Term, s)
    If (tf = 0) Then
        MsgBox "ERROR: Unable to initialize kwi", vbCritical
    End If

    ' Open the knowledgebase
    kbName = InputBox("KB Name (no extension):")
    s = "kwi(" + id + ", open($" + kbName + ".kb$), _)"
    tf = ExecStrLS(Term, s)
    If (tf = 0) Then
        MsgBox "ERROR: Unable to open .kb file", vbCritical
    End If
         
    ' Start a new session
    ' Now the session can be anything but kw_init, we use 'session_0'
    id = "'session_0'"
    tf = ExecStrLS(Term, "kwi(" + id + ", new_session, _)")
    If (tf = 0) Then
        MsgBox "ERROR: Unable to start new session", vbCritical
    End If
    
    ' There is more to begin with
    more = True
    
    ' Main execution loop
    ' Keep calling kwi / solve until there is nothing else to do
    While (ExecStrLS(solveTerm, "kwi(" + id + ", solve, _MORE)"))
        
        ' Check if we're done
        If (GetStrArgLS(solveTerm, 3) <> "more") Then
            more = False
        End If

        ' Process all the actions. Actions are either questions to
        ' ask the user or information to tell the user.
        Do
            ' Get the action
            If (ExecStrLS(actionTerm, "kwi(" + id + ", get_action, _ACTION)") = 0) Then
                MsgBox "ERROR: lbi / get_action failed", vbCritical
                GoTo StopRun
            End If
            
            ' The third parameter in the kwi call contains the type and
            ' parameters of the action to be performed
            ' Use the Amzi! Logic Server to retrieve the action
            Call GetArgLS(actionTerm, 3, bTERM, responseTerm)
            responseFunctor = GetFunctorLS(responseTerm)
            responseType = GetStrArgLS(responseTerm, 1)
            
            ' Get a fact from the user
            If (responseFunctor = "ask" And responseType = "user") Then
            
                ' The name of the fact we are seeking a value for
                factName = GetStrArgLS(responseTerm, 2)
                
                ' Get all the attributes of the fact. These are returned as a
                ' Prolog list that we convert to a collection
                Call GetArgLS(responseTerm, 3, bTERM, listTerm)
                Call prologListToCollection(listTerm, slotList)
                
                ' Fill in the blanks use an input box. All the slots from the
                ' question object are now in the slotList collection
                If (slotList.Item("question_type") = "fill_in_the_blank") Then
                    userResponse = InputBox(slotList.Item("prompt"), "Question: " + factName)
                    delimiter = "$"
                    If (slotList.Item("answer_type") = "text") Then
                        delimiter = "$"
                    End If
                    
                    ' Assert the new fact value using the KWI
                    If (ExecStrLS(Term, "kwi(" + id + ", assert(fact('" + factName + "', " + delimiter + userResponse + delimiter + ")), _)") = 0) Then
                        MsgBox "ERROR: kwi / assert failed", vbCritical
                    End If
                ' Menus use a list dialog box
                Else
                    If (slotList.Item("question_type") = "menu_single_choice") Then
                        listInput.Prompt = slotList.Item("prompt") + vbCrLf + "(Select one of the following)"
                    Else
                        listInput.Prompt = slotList.Item("prompt") + vbCrLf + "(Select one or more of the following)"
                    End If
                    
                    ' Fill the menu with the possible choices
                    listInput.menuList.Clear
                    Call StrToTermLS(Term, slotList.Item("choices"))
                    Call prologListToIndexedCollection(Term, choiceList)
                    For Each choice In choiceList
                        listInput.menuList.AddItem (choice)
                    Next choice
                    listInput.Show (1)
                    
                    ' Handle facts that have a single answer differently than those with
                    ' multiple answers. The latter must be passed back as Prolog lists,
                    ' enclosed in []'s.
                    If (slotList.Item("question_type") = "menu_single_choice") Then
                        listFacts = "$" + listInput.menuList.list(listInput.menuList.ListIndex) + "$"
                    End If
                    If (slotList.Item("question_type") = "menu_multiple_choices") Then
                        listFacts = "["
                        For i = 0 To listInput.menuList.ListCount - 1
                            If listInput.menuList.Selected(i) Then
                                listFacts = listFacts + "$" + listInput.menuList.list(i) + "$, "
                            End If
                        Next i
                        listFacts = Left(listFacts, Len(listFacts) - 2)
                        listFacts = listFacts + "]"
                    End If
                    
                    ' Assert the new fact value or values as composed above
                    If (ExecStrLS(Term, "kwi(" + id + ", assert(fact('" + factName + "', " + listFacts + ")), _)") = 0) Then
                        MsgBox "ERROR: kwi / assert failed", vbCritical
                    End If
                End If
            End If ' ask_user
            
            ' Display an answer for the user
            If (responseFunctor = "tell") Then
                
                ' Get all the attributes of the answer
                Call GetArgLS(responseTerm, 2, bTERM, listTerm)
                Call prologListToCollection(listTerm, slotList)
                
                ' Get the answer name
                answerName = slotList.Item("goal")
                
                ' Add the text to our solution box
                Solution.Text = Solution.Text + answerName + ": " + slotList.Item("text") + vbCrLf
                
            End If ' answer
            
        Loop While responseFunctor <> "none"
        
        If more = False Then
            GoTo StopRun
        End If
        
    Wend 'solveTerm

StopRun:
    ' If the KWI was started, close it
    If (id <> "none") Then
        Call ExecStrLS(actionTerm, "kwi(" + id + ", close, _)")
    End If
    
    ' Close the Amzi! Logic Server
    Call CloseLS
    Exit Sub
    
DisplayError:
    ' Since ErrorMethod is set to 1 (see top), then error 31300 is thrown by
    ' the Amzi! Logic Server. If the KWI is in use, we can ask it for details
    If (Err.Number = 31300 And id <> "none") Then
        If (ExecStrLS(Term, "kwi(" + id + ", get_error, _ERROR)") = 0) Then
            MsgBox "Error Processing Logic Server Error #: " + Str(ErrLS) + " -- " + ErrorLS
            Resume StopRun
        End If
        
        Call GetArgLS(Term, 3, bTERM, listTerm)
        Call prologListToIndexedCollection(listTerm, errorList)
        s = ""
        For Each entry In errorList
            s = s + entry + vbCrLf
        Next entry
        MsgBox s, vbCritical
    ' Otherwise it is an unexpected error in the Amzi! Logic Server
    Else
        Msg = "Logic Server Error #" + Str(ErrLS)
        Msg = Msg + " -- " + ErrorLS
        MsgBox Msg, vbCritical
    End If
      
    Resume StopRun
    
End Sub

' This functions doubles backslashes in a pathname before it is passed
' to the Amzi! Logic Server
Private Function doubleSlashes(ByVal path As String) As String
    Dim i As Integer, s As String
    
    s = ""
    For i = 1 To Len(path)
        If (Mid(path, i, 1) = "\") Then
            s = s + "\\"
        Else
            s = s + Mid(path, i, 1)
        End If
    Next i
    doubleSlashes = s
End Function

' This function converts a Prolog list to a Collection. In general, Prolog lists
' are expected to be flat and consist of ITEM = VALUE. If for some reason, a list
' element is itself a list, we quote the entire element so that it can be passed
' to StrToTermLS for further processing using the Amzi! Logic Server.
Private Sub prologListToCollection(ByVal slotList As Long, ByRef c As Collection)
    Dim element As Long, name As Long, value As Long, list As Long
    Dim rc As Integer, i As Integer
    Dim vstr As String, nstr As String
    
    list = slotList
    If (c.Count > 0) Then
        For i = 1 To c.Count
            c.Remove (1)
        Next i
    End If
    
    ' Check for the empty list or an atom
    If (GetTermTypeLS(list) <> pLIST) Then
        Exit Sub
    End If
    
    ' Otherwise get the head
    rc = GetHeadLS(list, bTERM, element)
    Call GetArgLS(element, 1, bTERM, name)
    Call GetArgLS(element, 2, bTERM, value)
    Call TermToStrLS(name, nstr, 1000)
    If (GetTermTypeLS(value) = pLIST) Then
        Call TermToStrQLS(value, vstr, 5000)
    Else
        Call TermToStrLS(value, vstr, 5000)
    End If
    c.Add vstr, nstr
    
    ' And the rest of the list
    list = GetTailLS(list)
    While (list <> 0)
        rc = GetHeadLS(list, bTERM, element)
        Call GetArgLS(element, 1, bTERM, name)
        Call GetArgLS(element, 2, bTERM, value)
        Call TermToStrLS(name, nstr, 1000)
        If (GetTermTypeLS(value) = pLIST) Then
            Call TermToStrQLS(value, vstr, 5000)
        Else
            Call TermToStrLS(value, vstr, 5000)
        End If
        c.Add vstr, nstr
        
        list = GetTailLS(list)
    Wend
    
End Sub

' This function takes a simple Prolog list and creates a collection where each element
' is numbered.
Private Sub prologListToIndexedCollection(ByVal slotList As Long, ByRef c As Collection)
    Dim element As Long, name As Long, value As Long, list As Long
    Dim rc As Integer, i As Integer
    Dim vstr As String
    
    list = slotList
    If (c.Count > 0) Then
        For i = 1 To c.Count
            c.Remove (1)
        Next i
    End If
    
    ' Check for the empty list or an atom
    If (GetTermTypeLS(list) <> pLIST) Then
        Exit Sub
    End If
    
    ' Otherwise get the head
    rc = GetHeadLS(list, bTERM, element)
    Call TermToStrLS(element, vstr, 5000)
    c.Add vstr
    
    ' And the rest of the list
    list = GetTailLS(list)
    While (list <> 0)
        rc = GetHeadLS(list, bTERM, element)
        Call TermToStrLS(element, vstr, 5000)
        c.Add vstr
        
        list = GetTailLS(list)
    Wend
    
End Sub

