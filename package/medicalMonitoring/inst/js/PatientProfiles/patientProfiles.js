// download all patient profiles in a zip file returned to the user
// pathpatientprofiles: array of strings with path to the patient profiles
// filelabel: (optional) string with label, file name is built as: 'patientProfiles-[label].zip'
// verbose: (optional) boolean, if true informative messages are printed in Console during execution
function downloadPatientProfiles(pathpatientprofiles, filelabel = null, verbose = false){

	var zip = new JSZip();
	if(verbose)	console.log("JSZip version", JSZip.version);
  
	// create name of the zip file
	zipName = 'patientProfiles'
	if(filelabel != null){
		filelabel = filelabel.replace(/[^a-z0-9]/gi, '_')//.toLowerCase()
		zipName = zipName + '-' + filelabel
	} 
	zipName = zipName + '.zip';

	if(verbose)
		console.log("Download patient profiles in zip:", zipName, "for files", pathpatientprofiles)
      
	var count = 0;   
	// forEach execute function on each element on an arry
	pathpatientprofiles.forEach(function (url) {

		//console.log("Export patient profile", count, ":", url);
      
		// pdf file should be imported as binary content
		JSZipUtils.getBinaryContent(url, function (err, data) {
          
			// extract file name from patient profile full path
			fileName = url.split(/[\\/]/).pop();
          
			// add content to current pdf file
			zip.file(fileName, data, { binary: true });

			// informative message in the console browser
 			count++;
	 		if(verbose)
				console.log("Export patient profile", count, ":", fileName);

			// create and returns the zip file to the user for the last file
			if (count == pathpatientprofiles.length) {
				if(verbose)	console.log("Create and upload zip file");
				zip.generateAsync({type:'blob'}).then(function(content) {
					saveAs(content, zipName);
				});
			};
        
		});
        
	});
 
};

// download patient profiles for a specific DT
// el: DT object
function downloadPatientProfilesDT(el){
    // extract patient profiles paths
    s = el.parentElement.innerHTML;// get row containing the button
    hrefArray = s.match(/href='[^']*/g); // extract the hyperlinks 
    linksArray = hrefArray.map(item => item.match(/href='([^']*)/)[1]); // and the patient profiles path
    // build label for file name
    //parentRow = el.closest('tr').previousSibling; // get parent row
    //console.log(el.closest('.dataTables_scroll')); // column names
    downloadPatientProfiles(pathpatientprofiles=linksArray, filelabel='');
};

// download patient profiles for a specific plot
// el: plotly object, with 'key' element containing patient IDs
// data: array of dictionaries, containing: 'key': patient IDs, 'path': path to patient profiles
// fromData: boolean, if true the id mapping is extracted from the 'data' slot of the plot object
// idvar: string, variable of the plot object considered for mapping
// labelplot: string, plot label (should map pl.data.set), used to restrict event to only specified plot
//  (to avoid that event triggered if a different plot uses the same key)
// labelvar: (optional) string with variable used to label the created zip file.
// If not specified and only one patient: patient ID is used; otherwise no label is used.
// verbose: (optional) boolean, if true informative messages are printed in Console during execution
function downloadPatientProfilesPlotly(el, x, data, fromdata, idvar, labelplot, labelvar = null, verbose = false) {

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

		iskey = key == 'p' //event.ctrlKey && 
		if(iskey & verbose)	console.log('p key pressed.')

		if(verbose){
			console.log('Plot object:', plObj);
			console.log('Extra data:', data);
			console.log('label plot:', labelplot);
		}

		if(plObj != null && iskey && (plObj.data.set == labelplot)){

			console.log('data.set:', plObj.data.set);

			// extract patient IDs
			if (fromdata){
				ids = plObj.data[idvar];
			}else{
				ids = plObj[idvar];
			}

			if(verbose){
				console.log('fromdata:', fromdata);
				console.log('from id var:', idvar);
				console.log('Selected IDs:', ids);
			}
			
			// filter data to only ids
			linksArray = data.filter(e => e.key.includes(ids))
			// split if multiple links are present
			linksArray = linksArray.map(el => el.path.split(', '));
			// flatten list of arrays
			linksArray = [].concat.apply([], linksArray);
			// remote hyperlink part if available
			//linksArray = linksArray.map(item => item.match(/href='([^']*)/)[1]);
			if(verbose)	console.log('Patient profile path:', linksArray);
			
			// extract label for the zip file name, here 'label' of the sunburst region
			if(verbose)	console.log("label var:", labelvar)
			if(labelvar != null){
				filelabel = plObj[labelvar];
			}else	if(ids.length == 1){
				filelabel = ids[0];
			}else{
				filelabel = null;
			}

			if(verbose)	console.log("file label:", filelabel)
			downloadPatientProfiles(pathpatientprofiles = linksArray, filelabel = filelabel);

		};
		
	});

};
