function downloadPatientProfiles(pathPatientProfiles, fileLabel){

  var zip = new JSZip();
  
      // create name of the zip file
      zipName='patientProfiles-' +fileLabel.replace(/[^a-z0-9]/gi, '_').toLowerCase()+ '.zip';
      
      var count = 0;   
      // forEach execute function on each element on an arry
      pathPatientProfiles.forEach(function (url) {
      
        // pdf file should be imported as binary content
        JSZipUtils.getBinaryContent(url, function (err, data) {
        
          // informative message in the console browser
          //console.log('Response from request ' + url); 
          count++;
          
          // extract file name from patient profile full path
          fileName = url.split(/[\\/]/).pop()
          
          // add content to current pdf file
          zip.file(fileName, data, { binary: true });
          
          // create and returns the zip file to the user for the last file
          if (count == pathPatientProfiles.length) {
            zip.generateAsync({type:'blob'}).then(function(content) {
              saveAs(content, zipName);
            });
          }
        
        });
        
      });
};

function downloadPatientProfilesDT(el){
    // extract patient profiles paths
    s = el.parentElement.innerHTML;// get row containing the button
    hrefArray = s.match(/href='[^']*/g); // extract the hyperlinks 
    linksArray = hrefArray.map(item => item.match(/href='([^']*)/)[1]); // and the patient profiles path
    // build label for file name
    //parentRow = el.closest('tr').previousSibling; // get parent row
    //console.log(el.closest('.dataTables_scroll')); // column names
    downloadPatientProfiles(pathPatientProfiles=linksArray, fileLabel='');
};

function downloadPatientProfilesPlotly(el, x, data) {

		// get plotly data from hover
		var plObj = null
		el.on('plotly_hover', function(d) {
			plObj = d.points[0];
		});
		// reset object (e.g. if go out of plotting region)
		el.on('plotly_unhover', function(data) {
			plObj = null;
		});

		// 'key' event not registered in plotly,
		// so create a custom DOM event
		window.addEventListener('keyup', function(event) {

			var key = event.key;

			console.log('pressed key', key)

			keyPress = event.altKey && key == 'p'
			if(keyPress){
				console.log('ALT + p key pressed.')
			}

			//alert('Export of the patient profiles in progress!');

			if(plObj != null && keyPress){

				// test to open a progress window:
				//var new_win = window.open('test.html','','menubar=no,resizable=yes,toolbar=no,status=no,hotkeys=no,titlebar=no,scrollbars=yes,height=50,width=50');
				//new_win.document.write('Create patient profiles');
				
				// string with patient profiles
				//links = plObj.customdata; 
				// convert to an array of patient profile path
				//linksArray = links.split(',');
				console.log(plObj);
				ids = plObj.data.key;
				console.log(ids);
				console.log(data);
				linksArray = data[ids].toArray();
				console.log(linksArray);
				
				// extract label for the zip file name, here 'label' of the sunburst region
				//plLabel=plObj.label;

				downloadPatientProfiles(pathPatientProfiles=linksArray, fileLabel = 'test');
				//new_win.close()

			};
		
		});

	};
