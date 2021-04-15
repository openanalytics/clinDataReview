// get patient profiles from array of paths to report
// pathpatientprofiles: array of strings with path to the patient profiles
// filelabel: (optional) string with label, file name is built as: 'patientProfiles-[label].zip'
// verbose: (optional) boolean, if true informative messages are printed in Console during execution
// download: (optional) boolean, if true (by default) the files are downloaded in a zip compressed file.
// If false and only one path is specified, the patient profile is opened in a new window.
function getPatientProfiles(pathpatientprofiles, filelabel = null, verbose = false, download = true){

	if(verbose)
		console.log("Get patient profiles", pathpatientprofiles, "(download =", download, ").")

	if (pathpatientprofiles.length == 1 && !download){

		if(verbose)
			console.log("Open patient profile:", pathpatientprofiles, "in a new window.")

		window.open(linksArray[0]);
		// to open in a new window:
		// window.open(linksArray[i], '_blank', 'toolbar=0,location=0,menubar=0');

	}else{
		if(!download)
			console.log('Patient profile is download in a zip file because multiple files are provided.')

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
		// forEach execute function on each element on an array
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

	}
 
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
// download: (optional) boolean, if true (by default) the files are downloaded in a compressed zip file.
// If false and only one path is specified, the patient profile is opened in a new window.
// verbose: (optional) boolean, if true informative messages are printed in Console during execution
function getPatientProfilesPlotly(el, x, data, fromdata, idvar, labelplot, labelvar = null, download = true, verbose = false) {

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
			console.log('download:', download);
		}

		if(plObj != null && iskey && (plObj.data.set == labelplot)){

			if(verbose){
				console.log('data.set:', plObj.data.set);
				console.log('fromdata:', fromdata);
				console.log('from id var:', idvar);
			}

			// extract patient IDs
			if (fromdata){
				ids = plObj.data[idvar];
			}else{
				ids = plObj[idvar];
			}

			// take distinct values (only if array, if idvar is of length 1: string object)
			if(Object.prototype.toString.call(ids) === '[object Array]')
				ids = [... new Set(ids)];
			// convert string to an array if only one element
			// otherwise filtering might select any elements with key containing a subpart of the id
			if (typeof ids === 'string')
				ids = new Array(ids);

			if(verbose)	console.log('Selected IDs:', ids);
			
			// filter data to only ids
			linksArray = data.filter(e => (ids.indexOf(e.key) > -1));
			if(verbose)	console.log('Patient profile paths filtered:', linksArray);
			// split if multiple links are present
			linksArray = linksArray.map(el => el.path.split(', '));
			if(verbose)	console.log('Patient profile paths split by ID:', linksArray);
			// flatten list of arrays
			linksArray = [].concat.apply([], linksArray);
			if(verbose)	console.log('Patient profile paths flattened to 1-level list:', linksArray);
			// remote hyperlink part if available
			//linksArray = linksArray.map(item => item.match(/href='([^']*)/)[1]);
			
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
			getPatientProfiles(pathpatientprofiles = linksArray, filelabel = filelabel, verbose = verbose, download = download);

		};
		
	});

};
