Option Strict Off
Option Explicit On 

Imports amzinet
Imports VB = Microsoft.VisualBasic

Friend Class mainform
	Inherits System.Windows.Forms.Form
#Region "Windows Form Designer generated code "
	Public Sub New()
		MyBase.New()
		If m_vb6FormDefInstance Is Nothing Then
			If m_InitializingDefInstance Then
				m_vb6FormDefInstance = Me
			Else
				Try 
					'For the start-up form, the first instance created is the default instance.
					If System.Reflection.Assembly.GetExecutingAssembly.EntryPoint.DeclaringType Is Me.GetType Then
						m_vb6FormDefInstance = Me
					End If
				Catch
				End Try
			End If
		End If
		'This call is required by the Windows Form Designer.
		InitializeComponent()
	End Sub
	'Form overrides dispose to clean up the component list.
	Protected Overloads Overrides Sub Dispose(ByVal Disposing As Boolean)
		If Disposing Then
			If Not components Is Nothing Then
				components.Dispose()
			End If
		End If
		MyBase.Dispose(Disposing)
	End Sub
	'Required by the Windows Form Designer
	Private components As System.ComponentModel.IContainer
	Public ToolTip1 As System.Windows.Forms.ToolTip
	Public WithEvents Solution As System.Windows.Forms.TextBox
	Public WithEvents HScroll1 As System.Windows.Forms.HScrollBar
	Public WithEvents Run As System.Windows.Forms.Button
	'NOTE: The following procedure is required by the Windows Form Designer
	'It can be modified using the Windows Form Designer.
	'Do not modify it using the code editor.
	<System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
		Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(mainform))
		Me.components = New System.ComponentModel.Container()
		Me.ToolTip1 = New System.Windows.Forms.ToolTip(components)
		Me.ToolTip1.Active = True
		Me.Solution = New System.Windows.Forms.TextBox
		Me.HScroll1 = New System.Windows.Forms.HScrollBar
		Me.Run = New System.Windows.Forms.Button
		Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
		Me.Text = "Run Logicbase"
		Me.ClientSize = New System.Drawing.Size(312, 211)
		Me.Location = New System.Drawing.Point(463, 111)
		Me.Font = New System.Drawing.Font("Arial", 8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
		Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
		Me.BackColor = System.Drawing.SystemColors.Control
		Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Sizable
		Me.ControlBox = True
		Me.Enabled = True
		Me.KeyPreview = False
		Me.MaximizeBox = True
		Me.MinimizeBox = True
		Me.Cursor = System.Windows.Forms.Cursors.Default
		Me.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.ShowInTaskbar = True
		Me.HelpButton = False
		Me.WindowState = System.Windows.Forms.FormWindowState.Normal
		Me.Name = "mainform"
		Me.Solution.AutoSize = False
		Me.Solution.Size = New System.Drawing.Size(233, 193)
		Me.Solution.Location = New System.Drawing.Point(8, 8)
		Me.Solution.MultiLine = True
		Me.Solution.ScrollBars = System.Windows.Forms.ScrollBars.Both
		Me.Solution.WordWrap = False
		Me.Solution.TabIndex = 2
		Me.Solution.Font = New System.Drawing.Font("Arial", 8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
		Me.Solution.AcceptsReturn = True
		Me.Solution.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.Solution.BackColor = System.Drawing.SystemColors.Window
		Me.Solution.CausesValidation = True
		Me.Solution.Enabled = True
		Me.Solution.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Solution.HideSelection = True
		Me.Solution.ReadOnly = False
		Me.Solution.Maxlength = 0
		Me.Solution.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.Solution.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Solution.TabStop = True
		Me.Solution.Visible = True
		Me.Solution.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
		Me.Solution.Name = "Solution"
		Me.HScroll1.Size = New System.Drawing.Size(2, 2)
		Me.HScroll1.Location = New System.Drawing.Point(8, 88)
		Me.HScroll1.TabIndex = 1
		Me.HScroll1.CausesValidation = True
		Me.HScroll1.Enabled = True
		Me.HScroll1.LargeChange = 1
		Me.HScroll1.Maximum = 32767
		Me.HScroll1.Minimum = 0
		Me.HScroll1.Cursor = System.Windows.Forms.Cursors.Default
		Me.HScroll1.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.HScroll1.SmallChange = 1
		Me.HScroll1.TabStop = True
		Me.HScroll1.Value = 0
		Me.HScroll1.Visible = True
		Me.HScroll1.Name = "HScroll1"
		Me.Run.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
		Me.Run.Text = "Run"
		Me.Run.Size = New System.Drawing.Size(57, 33)
		Me.Run.Location = New System.Drawing.Point(248, 8)
		Me.Run.TabIndex = 0
		Me.Run.Font = New System.Drawing.Font("Arial", 8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
		Me.Run.BackColor = System.Drawing.SystemColors.Control
		Me.Run.CausesValidation = True
		Me.Run.Enabled = True
		Me.Run.ForeColor = System.Drawing.SystemColors.ControlText
		Me.Run.Cursor = System.Windows.Forms.Cursors.Default
		Me.Run.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Run.TabStop = True
		Me.Run.Name = "Run"
		Me.Controls.Add(Solution)
		Me.Controls.Add(HScroll1)
		Me.Controls.Add(Run)
	End Sub
#End Region 
#Region "Upgrade Support "
	Private Shared m_vb6FormDefInstance As mainform
	Private Shared m_InitializingDefInstance As Boolean
	Public Shared Property DefInstance() As mainform
		Get
			If m_vb6FormDefInstance Is Nothing OrElse m_vb6FormDefInstance.IsDisposed Then
				m_InitializingDefInstance = True
				m_vb6FormDefInstance = New mainform()
				m_InitializingDefInstance = False
			End If
			DefInstance = m_vb6FormDefInstance
		End Get
		Set
			m_vb6FormDefInstance = Value
		End Set
	End Property
#End Region 
    ' KnowledgeWright Visual Basic .NET Interface Prototype
    ' Copyright (c) 2000-2002 Amzi! inc. All Rights Reserved.
	'
	' BEFORE RUNNING THIS PROGRAM, the following files in the 'workshop' subdirectory
    ' must be on your PATH environment variable or copied to the bin subdirectory:
    ' amzi.dll, amzinet.dll, aosutils.lsx, aodbc.lsx, amzi.cfg
    ' Also you need to copy the jig reasoning engine (.xpl) file from the 'workshop/jigs'
    ' subdirectory (e.g. basic.xpl for the basic jig), and a knowledgebase file (.kb)
    ' from the samples subdirectory to the bin subdirectory.
	'
	' This is the main function. It loads and runs a knowledgebase using the KWI.
	' The KWI is a Prolog language calling interface to KnowledgeWright, so all
	' the KWI calls are made through the Amzi! Logic Server.
	
	Private Sub Run_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Run.Click
        Dim ls As LogicServer
        Dim lsex As LSException
        Dim Msg As Object
		Dim answerName As Object
		Dim userResponse, responseFunctor, s, id, factName, delimiter As String
		Dim kbName, responseType, jig, listFacts As String
        Dim responseTerm, solveTerm, Term, actionTerm, listTerm As Long
		Dim tf, i As Short
		Dim slotList As New Collection
		Dim choiceList As New Collection
		Dim errorList As New Collection
		Dim more As Boolean
		Dim choice, entry As Object

        ' Initialize the Amzi! Logic Server and load the jig runtime (.xpl file)
        ' There is no KWI session id right now. This is used in the error handler
        ' to determine whether the error comes from the Amzi! Logic Server or the
        ' KnowledgeWright KWI.
        id = "none"

		' Initialize a new Logic Server
        ls = New LogicServer()
        Try
            ls.Init("")

            ' Tell the Logic Server to load any LSX's that are specified in the amzi.cfg file
            ' Both aosutils and aodbc are required
            ls.AddLSX("aosutils")
            ls.AddLSX("aodbc")

            ' Get the name of the jig to run (an Amzi! Prolog .xpl binary program file)
            jig = InputBox("Jig Runtime Name (no extension):")
            s = (VB6.GetPath & "\" & jig & ".xpl")
            ls.Load(s)

            ' Initialize the runtime kwi
            ' Prior to starting a new KWI session the session id must be kw_init.
            id = "kw_init"
            ' The directory is where the kb file is located
            ' The session_directory is where a temporary file is created to maintain the state
            '   of the reasoning process
            ' The log_file is a log of the reasoning process and is created in the directory
            ' The message level is the level of detail in the log_file
            ' The debug_msgs file is for internal debugging messages
            s = "kwi(" & id & ", initialize([directory = $" & doubleSlashes(VB6.GetPath) & "\\$, session_directory = $" & doubleSlashes(VB6.GetPath) & "\\$, log_file = $vbrun.log$, debug_msgs = $vbdebug.log$, message_level = high]), _)"
            Term = ls.ExecStr(s)
            If (Term = 0) Then
                MsgBox("ERROR: Unable to initialize kwi", MsgBoxStyle.Critical)
            End If

            ' Open the knowledgebase
            kbName = InputBox("KB Name (no extension):")
            s = "kwi(" & id & ", open($" & kbName & ".kb$), _)"
            Term = ls.ExecStr(s)
            If (Term = 0) Then
                MsgBox("ERROR: Unable to open .kb file", MsgBoxStyle.Critical)
            End If

            ' Start a new session
            ' Now the session can be anything but kw_init, we use 'session_0'
            id = "'session_0'"
            Term = ls.ExecStr("kwi(" & id & ", new_session, _)")
            If (Term = 0) Then
                MsgBox("ERROR: Unable to start new session", MsgBoxStyle.Critical)
            End If

            ' There is more to begin with
            more = True

            ' Main execution loop
            ' Keep calling kwi / solve until there is nothing else to do
            solveTerm = ls.ExecStr("kwi(" & id & ", solve, _MORE)")
            While (solveTerm <> 0)

                ' Check if we're done
                If (ls.GetStrArg(solveTerm, 3) <> "more") Then
                    more = False
                End If

                ' Process all the actions. Actions are either questions to
                ' ask the user or information to tell the user.
                Do
                    ' Get the action
                    actionTerm = ls.ExecStr("kwi(" & id & ", get_action, _ACTION)")
                    If (actionTerm = 0) Then
                        MsgBox("ERROR: lbi / get_action failed", MsgBoxStyle.Critical)
                        GoTo StopRun
                    End If

                    ' The third parameter in the kwi call contains the type and
                    ' parameters of the action to be performed
                    ' Use the Amzi! Logic Server to retrieve the action
                    responseTerm = ls.GetArg(actionTerm, 3)
                    responseFunctor = ls.GetFunctor(responseTerm)
                    responseType = ls.GetStrArg(responseTerm, 1)

                    ' Get a fact from the user
                    If (responseFunctor = "ask" And responseType = "user") Then

                        ' The name of the fact we are seeking a value for
                        factName = ls.GetStrArg(responseTerm, 2)

                        ' Get all the attributes of the fact. These are returned as a
                        ' Prolog list that we convert to a collection
                        listTerm = ls.GetArg(responseTerm, 3)
                        Call prologListToCollection(ls, listTerm, slotList)

                        ' Fill in the blanks use an input box. All the slots from the
                        ' question object are now in the slotList collection
                        s = slotList.Item("question_type")
                        If (slotList.Item("question_type") = "fill_in_the_blank") Then
                            userResponse = InputBox(slotList.Item("prompt"), "Question: " & factName)
                            delimiter = "$"
                            If (slotList.Item("answer_type") = "text") Then
                                delimiter = "$"
                            End If

                            ' Assert the new fact value using the KWI
                            Term = ls.ExecStr("kwi(" & id & ", assert(fact('" & factName & "', " & delimiter & userResponse & delimiter & ")), _)")
                            If (Term = 0) Then
                                MsgBox("ERROR: kwi / assert failed", MsgBoxStyle.Critical)
                            End If
                            ' Menus use a list dialog box
                        Else
                            If (slotList.Item("question_type") = "menu_single_choice") Then
                                listInput.DefInstance.Prompt.Text = slotList.Item("prompt") + vbCrLf + "(Select one of the following)"
                            Else
                                listInput.DefInstance.Prompt.Text = slotList.Item("prompt") + vbCrLf + "(Select one or more of the following)"
                            End If

                            ' Fill the menu with the possible choices
                            listInput.DefInstance.menuList.Items.Clear()
                            Term = ls.StrToTerm(slotList.Item("choices"))
                            Call prologListToIndexedCollection(ls, Term, choiceList)
                            For Each choice In choiceList
                                listInput.DefInstance.menuList.Items.Add((choice))
                            Next choice
                            VB6.ShowForm(listInput.DefInstance, (1))

                            ' Handle facts that have a single answer differently than those with
                            ' multiple answers. The latter must be passed back as Prolog lists,
                            ' enclosed in []'s.
                            If (slotList.Item("question_type") = "menu_single_choice") Then
                                listFacts = "$" & VB6.GetItemString(listInput.DefInstance.menuList, listInput.DefInstance.menuList.SelectedIndex) & "$"
                            End If
                            If (slotList.Item("question_type") = "menu_multiple_choices") Then
                                listFacts = "["
                                For i = 0 To listInput.DefInstance.menuList.Items.Count - 1
                                    If listInput.DefInstance.menuList.GetSelected(i) Then
                                        listFacts = listFacts & "$" & VB6.GetItemString(listInput.DefInstance.menuList, i) & "$, "
                                    End If
                                Next i
                                listFacts = VB.Left(listFacts, Len(listFacts) - 2)
                                listFacts = listFacts & "]"
                            End If

                            ' Assert the new fact value or values as composed above
                            Term = ls.ExecStr("kwi(" & id & ", assert(fact('" & factName & "', " & listFacts & ")), _)")
                            If (Term = 0) Then
                                MsgBox("ERROR: kwi / assert failed", MsgBoxStyle.Critical)
                            End If
                        End If
                    End If ' ask_user

                    ' Display an answer for the user
                    If (responseFunctor = "tell") Then

                        ' Get all the attributes of the answer
                        listTerm = ls.GetArg(responseTerm, 2)
                        Call prologListToCollection(ls, listTerm, slotList)

                        ' Get the answer name
                        answerName = slotList.Item("goal")

                        ' Add the text to our solution box
                        Solution.Text = Solution.Text + answerName + ": " + slotList.Item("text") + vbCrLf

                    End If ' answer

                Loop While responseFunctor <> "none"

                If more = False Then
                    GoTo StopRun
                End If

                solveTerm = ls.ExecStr("kwi(" & id & ", solve, _MORE)")

            End While 'solveTerm

StopRun:
            ' If the KWI was started, close it
            If (id <> "none") Then
                actionTerm = ls.ExecStr("kwi(" & id & ", close, _)")
            End If

            ' Close the Amzi! Logic Server
            ls.Close()

        Catch ex As LSException
            If (ex.GetType() = exTYPE.FATAL) Then
                MsgBox(ex.GetMsg(), MsgBoxStyle.Critical, "")
            Else
                MsgBox(ex.GetMsg(), MsgBoxStyle.Exclamation, "")
            End If
        End Try

    End Sub

    ' This functions doubles backslashes in a pathname before it is passed
    ' to the Amzi! Logic Server
    Private Function doubleSlashes(ByVal path As String) As String
        Dim i As Short
        Dim s As String

        s = ""
        For i = 1 To Len(path)
            If (Mid(path, i, 1) = "\") Then
                s = s & "\\"
            Else
                s = s & Mid(path, i, 1)
            End If
        Next i
        doubleSlashes = s
    End Function

    ' This function converts a Prolog list to a Collection. In general, Prolog lists
    ' are expected to be flat and consist of ITEM = VALUE. If for some reason, a list
    ' element is itself a list, we quote the entire element so that it can be passed
    ' to StrToTermLS for further processing using the Amzi! Logic Server.
    Private Sub prologListToCollection(ByRef ls As LogicServer, ByVal slotList As Long, ByRef c As Collection)
        Dim value, element, fname, list, newlist, type As Long
        Dim rc, i As Short
        Dim vstr, nstr, s As String

        list = slotList
        If (c.Count() > 0) Then
            For i = 1 To c.Count()
                c.Remove((1))
            Next i
        End If

        ' Check for the empty list or an atom
        type = ls.GetTermType(list)
        If (type <> pTYPE.pLIST) Then
            Exit Sub
        End If

        ' Otherwise get the head
        element = ls.GetHead(list)
        fname = ls.GetArg(element, 1)
        value = ls.GetArg(element, 2)
        nstr = ls.TermToStr(fname, 1000)
        If (ls.GetTermType(value) = pTYPE.pLIST) Then
            vstr = ls.TermToStrQ(value, 5000)
        Else
            vstr = ls.TermToStr(value, 5000)
        End If
        c.Add(vstr, nstr)

        ' And the rest of the list
        list = ls.GetTail(list)
        While (list <> 0)
            element = ls.GetHead(list)
            fname = ls.GetArg(element, 1)
            value = ls.GetArg(element, 2)
            nstr = ls.TermToStr(fname, 1000)
            If (ls.GetTermType(value) = pTYPE.pLIST) Then
                vstr = ls.TermToStrQ(value, 5000)
            Else
                vstr = ls.TermToStr(value, 5000)
            End If
            c.Add(vstr, nstr)

            list = ls.GetTail(list)
        End While

    End Sub

    ' This function takes a simple Prolog list and creates a collection where each element
    ' is numbered.
    Private Sub prologListToIndexedCollection(ByRef ls As LogicServer, ByVal slotList As Long, ByRef c As Collection)
        Dim value, element, name_Renamed, list As Long
        Dim rc, i As Short
        Dim vstr As String

        list = slotList
        If (c.Count() > 0) Then
            For i = 1 To c.Count()
                c.Remove((1))
            Next i
        End If

        ' Check for the empty list or an atom
        If (ls.GetTermType(list) <> pTYPE.pLIST) Then
            Exit Sub
        End If

        ' Otherwise get the head
        element = ls.GetHead(list)
        vstr = ls.TermToStr(element, 5000)
        c.Add(vstr)

        ' And the rest of the list
        list = ls.GetTail(list)
        While (list <> 0)
            element = ls.GetHead(list)
            vstr = ls.TermToStr(element, 5000)
            c.Add(vstr)

            list = ls.GetTail(list)
        End While

    End Sub
End Class