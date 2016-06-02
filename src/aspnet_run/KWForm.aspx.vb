Imports System.Collections.Specialized
Imports System.IO

Imports amzinet

Public Class KWForm
    Inherits System.Web.UI.Page
    Protected WithEvents KWPanel As System.Web.UI.WebControls.Panel
    Protected WithEvents InOut As System.Web.UI.HtmlControls.HtmlGenericControl

#Region " Web Form Designer Generated Code "

    'This call is required by the Web Form Designer.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()

    End Sub

    Private Sub Page_Init(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Init
        'CODEGEN: This method call is required by the Web Form Designer
        'Do not modify it using the code editor.
        InitializeComponent()
    End Sub

#End Region

    Private Sub Page_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim id, charset As String
        Dim ls As LogicServer

        'Get a unique session id for kw (also in Global.axax.vb/Session_End)
        id = "kw_" + Session.SessionID

        'We've been here before
        If Page.IsPostBack Then
            'Set the character set
            charset = Session("kw.charset")
            response.Charset = charset

            'Get our LogicServer instance
            ls = Session("kw.logicserver")

            'Assert the new facts
            assertFacts(ls, id)

            'And resume knowledgebase execution
            runKB(ls, id)

        Else
            'First time through
            startKB(id, False)
        End If
    End Sub

    Private Sub assertFacts(ByVal ls As LogicServer, ByVal id As String)
        Dim facts As NameValueCollection
        Dim i, j As Integer
        Dim name, factList, values(), s As String
        Dim term As Long

        Try
            'Load Form variables into NameValueCollection variable.
            facts = Request.Form

            'Backup in case the user went back to a prior form
            For i = 0 To facts.Count - 1
                name = facts.GetKey(i)
                If Not (name.StartsWith("__")) And name.ToUpper() <> "SUBMIT" Then
                    If ls.ExecStr("kwi('" + id + "', backup(fact('" + name + "', _)), _)") = 0 Then
                        Throw New Exception("kwi / backup to fact " + name + " failed")
                    End If
                End If
            Next

            'Assert the new data
            For i = 0 To facts.Count - 1
                name = facts.GetKey(i)
                If Not (name.StartsWith("__")) And name.ToUpper() <> "SUBMIT" Then
                    values = facts.GetValues(i)

                    If values.Length = 1 Then
                        term = ls.ExecStr("kwi('" + id + "', assert(fact('" + name + "', $" + values(0) + "$)), _)")
                        If term = 0 Then
                            Throw New Exception("kwi / assert failed for fact " + name + " value " + values(0))
                        End If
                    Else
                        factList = ""
                        For j = 0 To values.Length - 1
                            factList = factList + "$" + values(i) + "$,"
                        Next
                        s = "kwi('" + id + "', assert(fact('" + name + "', [" + factList.Substring(0, factList.Length() - 1) + "])), _)"
                        If ls.ExecStr(s) = 0 Then
                            Throw New Exception("kwi /assert failed for fact " + name + " value " + factList)
                        End If
                    End If
                End If
            Next
        Catch ex As LSException
            InOut.InnerHtml = formatKBError(ex, ls, "'" + id + "'")
            Return
        End Try

    End Sub

    Private Sub startKB(ByVal id As String, ByVal backup As Boolean)
        Dim ls As LogicServer
        Dim zid, path, kbname, jig, charset, s As String
        Dim term, charsetTerm As Long
        Dim params As NameValueCollection
        Dim din As StreamReader

        Try
            'Create a LogicServer for this user and save it in the session data
            ls = New LogicServer()
            Session("kw.logicserver") = ls
            ls.Init("")
            ls.AddLSX("aosutils.lsx")

            'Determine where we are
            path = Request.PhysicalApplicationPath

            'Determine the jig from the kb file
            params = Request.QueryString
            kbname = params.Get("kb_file")
            din = File.OpenText(path + kbname)
            s = din.ReadLine()
            If s.Length() = 0 Then
                InOut.InnerHtml = "Unable to find jig name in kb file"
                Return
            Else
                jig = s.Substring(s.IndexOf("(") + 1, (s.IndexOf(",") - s.IndexOf("(")) - 1)
            End If

            'Load the jig
            ls.Load(path + jig + ".xpl")

            'Initialize the runtime with directory and logfile
            'Prior to starting a new KWI session the session id must be kw_init.
            zid = "kw_init"

            'The directory is where the kb file is located
            'The session_directory is where a temporary file is created to maintain the state
            'of the reasoning process
            'The log_file is a log of the reasoning process and is created in the directory
            'The message level is the level of detail in the log_file
            s = "kwi(" + zid + ", initialize([directory = $" + doubleSlashes(path) + "$, "
            If params.Get("temp_directory") = Nothing Then
                s = s + "session_directory = $" + doubleSlashes(path) + "bin\\$, "
            Else
                s = s + "session_directory = $" + doubleSlashes(params.Get("temp_directory")) + "\\$,"
            End If
            If params.Get("log_file") <> Nothing Then
                s = s + "log_file = $" + params.Get("log_file") + "$, message_level = high, "
            Else
                s = s + "message_level = none, "
            End If
            s = s + "session_save = true]), _INFO)"
            term = ls.ExecStr(s)

            s = "kwi(" + zid + ", open($" + kbname + "$), _)"
            term = ls.ExecStr(s)
        Catch ex As LSException
            InOut.InnerHtml = "startKB / " + formatKBError(ex, ls, "'" + zid + "'")
            Return
        End Try

        Try
            'Create a new session
            term = ls.ExecStr("kwi('" + id + "', new_session, _)")

            'Get the character set
            charsetTerm = ls.ExecStr("kwi('" + id + "', get_parm(knowledgebase, main, charset), _CHARSET)")
            If charsetTerm <> 0 Then
                charset = ls.GetStrArg(charsetTerm, 3)
                Session("kw.charset") = charset
                Response.Charset = charset
            End If
        Catch ex As LSException
            InOut.InnerHtml = "startKB / " + formatKBError(ex, ls, "'" + id + "'")
            Return
        End Try

        runKB(ls, id)

    End Sub

    Private Sub runKB(ByVal ls As LogicServer, ByVal id As String)
        Dim solveTerm, actionTerm, responseTerm As Long
        Dim responseFunctor, responseType, answerName, solution As String
        Dim question, answer As Collection
        Dim more As Boolean

        Try
            'Call solve to get the next set of actions
            more = True
            solveTerm = ls.ExecStr("kwi('" + id + "', solve, _MORE)")
            If solveTerm <> 0 Then

                ' Check if we're done
                If ls.GetStrArg(solveTerm, 3) = "more" Then
                    more = False
                End If

                Do
                    'Get the action
                    actionTerm = ls.ExecStr("kwi('" + id + "', get_action, _ACTION)")
                    If actionTerm = 0 Then
                        Throw New Exception("kwi / get_action failed")
                    End If

                    'The third parameter in the kwi call contains the type and
                    'parameters of the action to be performed
                    'Use the Amzi! Logic Server to retrieve the action
                    responseTerm = ls.GetArg(actionTerm, 3)
                    responseFunctor = ls.GetFunctor(responseTerm)
                    responseType = ls.GetStrArg(responseTerm, 1)

                    'Display the HTML question page (we ignore this...used by the web interface)
                    If responseFunctor = "ask" And responseType = "html" Then
                        'Return the html formatted questions
                        question = New Collection()
                        prologListToCollection(ls, ls.GetArg(responseTerm, 3), question)

                        'Output them
                        InOut.InnerHtml = question.Item("html")

                    End If

                    'Display an answer
                    If responseFunctor = "tell" And responseType = "user" Then

                        'Get all the slots in the answer into a Properties object
                        answer = New Collection()
                        prologListToCollection(ls, ls.GetArg(responseTerm, 2), answer)

                        'Get the name of the answer
                        answerName = answer.Item("goal")

                        'Output the solution
                        solution = answer.Item("text")
                        If solution.Length > 0 Then
                            InOut.InnerHtml = solution
                        Else
                            InOut.InnerHtml = "No text for goal: " + answerName
                        End If
                    End If

                Loop While responseFunctor <> "none"
            End If 'solveTerm <> 0

            'Consider whether to close the KWI and LogicServer if more is false

        Catch ex As LSException
            InOut.InnerHtml = "runKB / " + formatKBError(ex, ls, "'" + id + "'")
            Return
        End Try

    End Sub

    'Display errors from the KWI or the underlying Amzi! Logic Server
    Private Function formatKBError(ByVal e As LSException, ByVal ls As LogicServer, ByVal id As String) As String
        Dim s, msg, name, lineno As String
        Dim term, errorList As Long
        Dim i As Integer
        Dim info As New Collection()

        Try
            'KBI errors return a detailed list of attributes
            If e.GetMsg().IndexOf("kwi_error(") >= 0 Then
                s = "kwi(" + id + ", get_error, _ERROR)"
                term = ls.ExecStr(s)

                'If we can't get error information, don't die
                If term = 0 Then
                    msg = "Unable to call kwi / get_error for: " + e.GetMsg()
                    Return (msg)
                End If

                'Get all the attributes of the error into a Properties object
                prologListToIndexedCollection(ls, ls.GetArg(term, 3), info)

                'Build a nicely formatted message
                msg = ""
                For i = 1 To info.Count
                    msg = msg + info.Item(i) + "<p>"
                Next
                If msg <> "" Then
                    Return ("<h3>" + msg + "</h3>")
                End If
            End If

            'Consult errors (outside the KWI)
            If e.GetType() = exTYPE.READ Then
                lineno = e.GetReadLineno().ToString()
                msg = e.GetMsg() + "<p> in file " + e.GetReadFileName() + "<p> at line " + lineno + "<p>" + e.GetReadBuffer()
                Return ("<h3>" + msg + "</h3>")
            End If

            'Unrecognized Logic Server error
            If msg = "" Then
                Return ("<h3>" + e.GetMsg() + "</h3>")
            End If
        Catch ex2 As LSException
            Return ("<h3>Error catching error</h3>")
        End Try
    End Function


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
