Option Strict Off
Option Explicit On
Friend Class listInput
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
	Public WithEvents menuList As System.Windows.Forms.ListBox
	Public WithEvents Prompt As System.Windows.Forms.TextBox
	Public WithEvents OKButton As System.Windows.Forms.Button
	'NOTE: The following procedure is required by the Windows Form Designer
	'It can be modified using the Windows Form Designer.
	'Do not modify it using the code editor.
	<System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
		Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(listInput))
		Me.components = New System.ComponentModel.Container()
		Me.ToolTip1 = New System.Windows.Forms.ToolTip(components)
		Me.ToolTip1.Active = True
		Me.menuList = New System.Windows.Forms.ListBox
		Me.Prompt = New System.Windows.Forms.TextBox
		Me.OKButton = New System.Windows.Forms.Button
		Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
		Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
		Me.Text = "Ask User"
		Me.ClientSize = New System.Drawing.Size(402, 213)
		Me.Location = New System.Drawing.Point(184, 250)
		Me.MaximizeBox = False
		Me.MinimizeBox = False
		Me.ShowInTaskbar = False
		Me.Font = New System.Drawing.Font("Arial", 8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
		Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
		Me.BackColor = System.Drawing.SystemColors.Control
		Me.ControlBox = True
		Me.Enabled = True
		Me.KeyPreview = False
		Me.Cursor = System.Windows.Forms.Cursors.Default
		Me.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.HelpButton = False
		Me.WindowState = System.Windows.Forms.FormWindowState.Normal
		Me.Name = "listInput"
		Me.menuList.Size = New System.Drawing.Size(297, 124)
		Me.menuList.Location = New System.Drawing.Point(8, 80)
		Me.menuList.SelectionMode = System.Windows.Forms.SelectionMode.MultiSimple
		Me.menuList.TabIndex = 2
		Me.menuList.Font = New System.Drawing.Font("Arial", 8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
		Me.menuList.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
		Me.menuList.BackColor = System.Drawing.SystemColors.Window
		Me.menuList.CausesValidation = True
		Me.menuList.Enabled = True
		Me.menuList.ForeColor = System.Drawing.SystemColors.WindowText
		Me.menuList.IntegralHeight = True
		Me.menuList.Cursor = System.Windows.Forms.Cursors.Default
		Me.menuList.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.menuList.Sorted = False
		Me.menuList.TabStop = True
		Me.menuList.Visible = True
		Me.menuList.MultiColumn = False
		Me.menuList.Name = "menuList"
		Me.Prompt.AutoSize = False
		Me.Prompt.Size = New System.Drawing.Size(297, 65)
		Me.Prompt.Location = New System.Drawing.Point(8, 8)
		Me.Prompt.MultiLine = True
		Me.Prompt.ScrollBars = System.Windows.Forms.ScrollBars.Both
		Me.Prompt.WordWrap = False
		Me.Prompt.TabIndex = 1
		Me.Prompt.Font = New System.Drawing.Font("Arial", 8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
		Me.Prompt.AcceptsReturn = True
		Me.Prompt.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.Prompt.BackColor = System.Drawing.SystemColors.Window
		Me.Prompt.CausesValidation = True
		Me.Prompt.Enabled = True
		Me.Prompt.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Prompt.HideSelection = True
		Me.Prompt.ReadOnly = False
		Me.Prompt.Maxlength = 0
		Me.Prompt.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.Prompt.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Prompt.TabStop = True
		Me.Prompt.Visible = True
		Me.Prompt.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
		Me.Prompt.Name = "Prompt"
		Me.OKButton.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
		Me.OKButton.Text = "OK"
		Me.OKButton.Size = New System.Drawing.Size(81, 25)
		Me.OKButton.Location = New System.Drawing.Point(312, 8)
		Me.OKButton.TabIndex = 0
		Me.OKButton.Font = New System.Drawing.Font("Arial", 8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
		Me.OKButton.BackColor = System.Drawing.SystemColors.Control
		Me.OKButton.CausesValidation = True
		Me.OKButton.Enabled = True
		Me.OKButton.ForeColor = System.Drawing.SystemColors.ControlText
		Me.OKButton.Cursor = System.Windows.Forms.Cursors.Default
		Me.OKButton.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.OKButton.TabStop = True
		Me.OKButton.Name = "OKButton"
		Me.Controls.Add(menuList)
		Me.Controls.Add(Prompt)
		Me.Controls.Add(OKButton)
	End Sub
#End Region 
#Region "Upgrade Support "
	Private Shared m_vb6FormDefInstance As listInput
	Private Shared m_InitializingDefInstance As Boolean
	Public Shared Property DefInstance() As listInput
		Get
			If m_vb6FormDefInstance Is Nothing OrElse m_vb6FormDefInstance.IsDisposed Then
				m_InitializingDefInstance = True
				m_vb6FormDefInstance = New listInput()
				m_InitializingDefInstance = False
			End If
			DefInstance = m_vb6FormDefInstance
		End Get
		Set
			m_vb6FormDefInstance = Value
		End Set
	End Property
#End Region 
	
	
	Private Sub OKButton_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles OKButton.Click
		Hide()
	End Sub
End Class