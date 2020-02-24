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

// el: plotly object, with 'key' element containing patient IDs
// data: array of dictionaries, containing: 'key': patient IDs, 'path': path to patient profiles
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

			keyPress = event.ctrlKey && key == 'Enter'
			if(keyPress){
				console.log('Ctrl + Enter key pressed.')
			}
			//console.log(plObj)
			//alert('Export of the patient profiles in progress!');

			if(plObj != null && keyPress){

        // extract patient IDs (passed as 'key' of plotly object)
				ids = plObj.data["key"];
				console.log('Selected IDs:', ids);
				
				// filter data to only ids
				linksArray = data.filter(e => e.key.includes(ids)).map(el => el.path);
				console.log('Patient profile path:', linksArray);
				
				// extract label for the zip file name, here 'label' of the sunburst region
				//plLabel=plObj.label;

				downloadPatientProfiles(pathPatientProfiles=linksArray, fileLabel = 'test');
				//new_win.close()

			};
		
		});

	};
