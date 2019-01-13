const fbdb = (function() {
	// Initialize database
	const db = firebase.firestore();

	// Disable deprecated features
	db.settings({
		timestampsInSnapshots: true
	});

	// Return functions for db operations
	return {
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