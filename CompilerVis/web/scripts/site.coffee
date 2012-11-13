editor = null

$ ->
	modifyingQS = (url) -> "#{url}?#{Date.now()}"

	loadTest = (name) ->
		$.ajax
			type: 'GET'
			url: modifyingQS ('/api/tests/' + name)
			success: (data) ->
				editor.setValue(data, -1)

	reloadTests = ->
		$.ajax
			type: 'GET'
			url: modifyingQS  '/api/tests'
			success: (data) ->
				$ddm = $('#test-dropdown')
				$ddm.empty()
				for test in data
					$ddm.append "<li><a href='#'>#{test}</a></li>"

				$ddm.find('a').click (e) ->
					loadTest $(this).text()

	compile = (code) ->
		$.ajax
			type: 'POST'
			data:
				code: code
			dataType: 'json'
			url: modifyingQS '/api/compiler'
			success: (data) ->
				for k, v of data
					$('#'+k+'-output').text v

	# tabify the output tabs
	$('#output-tabs a').click (e) ->
		e.preventDefault()
		$(this).tab('show')

	# reload the test names
	$('#test-btn').click (e) ->
		reloadTests()

	# compile it
	$('#compile-btn').click (e) ->
		compile editor.getValue()

	# ace stuff
	editor = ace.edit 'source-text'
	editor.setTheme 'ace/theme/chrome'
	editor.setShowPrintMargin(false)
