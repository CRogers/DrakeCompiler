$ ->
	loadTest = (name) ->
		$.ajax
			method: 'get'
			url: '/api/tests/' + name
			success: (data) ->
				$('#source-text').text data

	reloadTests = ->
		$.ajax
			method: 'get'
			url: '/api/tests'
			success: (data) ->
				$ddm = $('#test-dropdown')
				$ddm.empty()
				for test in data
					$ddm.append "<li><a href='#'>#{test}</a></li>"

				$ddm.find('a').click (e) ->
					loadTest $(this).text()

	# tabify the output tabs
	$('#output-tabs a').click (e) ->
		e.preventDefault()
		$(this).tab('show')

	# reload the test names
	$('#test-btn').click (e) ->
		reloadTests()

	# compile it
	#$('#compile-btn')