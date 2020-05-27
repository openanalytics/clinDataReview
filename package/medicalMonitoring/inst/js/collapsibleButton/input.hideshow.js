window.onload = function() {

	$( "input.hideshow" ).each( function(index, button) {
	//button.value = 'Show plot button';
		console.log(this);
		var target = this.nextSibling ? this : this.parentNode;
		target = target.nextSibling.nextSibling;
console.log(target);
		target.style.display = 'none';
		$( button ).click( function () {
			if ( target.style.display == 'none') {// || target.style.display == '' 
				target.style.display = 'inline-block'; // div should fit content size
				//window.dispatchEvent(new Event("resize"));
				//this.value = 'Show plot button';
			} else {
				target.style.display = 'none';
				//target.
				//this.value = 'Hide profile plot';
			}
		});
	});

}
