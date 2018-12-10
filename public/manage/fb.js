const fbdb = (function() {
	const config = {
		apiKey: "AIzaSyA7yISzO1I4rss7s1mykVmBjMAKGDExRjo",
		authDomain: "writer-788cf.firebaseapp.com",
		databaseURL: "https://writer-788cf.firebaseio.com",
		projectId: "writer-788cf",
		storageBucket: "writer-788cf.appspot.com",
		messagingSenderId: "181941734046"
	};
	  
	firebase.initializeApp(config);

	// Initialize database
	const db = firebase.firestore();

	// Disable deprecated features
	db.settings({
		timestampsInSnapshots: true
	});

	// Return functions for db operations
	return {
		saveToDb: data => {
			db.collection(data.collection).doc(data.id).set(data.content)
				.then(() => {
					console.log("Document written");
				})
				.catch(error => {
					console.error("Error adding document: ", error);
				});
		},

		saveBatchToDb: data => {
			const batch = db.batch();
			
			data.forEach(item => {
				let ref = db.collection(item.collection).doc(item.id);
				batch.set(ref, item.data);
			});
			
			batch.commit()
				.then(() => {
					console.log("Batch written");
				})
				.catch(error => {
					console.error("Error writing batch: ", error);
				});
		},

		queryDb: query => {
			const where = query.hasOwnProperty('where') ? query.where : ''

			db.collection(query.collection)
				.get()
				.where(where)
				.then(querySnapshot => {
					querySnapshot.forEach(doc => {
						console.log(doc.name + ', ' + doc.minutes + ' minutes.');
					});
				})
				.catch(error => {
					consol.log("Error getting documents: ", error);
				});
		}
	};

}());