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
				.catch(error => {
					console.error("Error writing batch: ", error);
				});
		},

		queryDb: query => {
			return db.collection(query.collection)
 				.get()
				.then(querySnapshot => {
					const data = [];

					querySnapshot.forEach(doc => {
						data.push(doc.data());
					});

					return data;
				})
				.catch(error => {
					consol.log("Error getting documents: ", error);
				});
		}
	};

}());