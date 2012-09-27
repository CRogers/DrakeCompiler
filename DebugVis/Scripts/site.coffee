$ ->
	# tabify the output tabs
	$('#output-tabs a').click (e) ->
		e.preventDefault()
		$(this).tab('show')