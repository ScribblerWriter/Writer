const fbdb = (function(messageCallback) {
	// Initialize database
	const db = firebase.firestore();

	// Return functions for db operations
	return {
		queryDbMultiple: query => {
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
					console.log("Error getting documents: ", error);
				});
		},

		queryDbSingle: query => {
			return db.collection(query.collection).doc(query.doc)
				.get()
				.then(doc => {
					if(doc.exists) {
						return doc.data();
					} else {
						console.log("No such document: ", query.doc);
					}
				})
				.catch(error => {
					console.log("Error getting document: ", error);
				});

		},

		queryDbSingleSubCollection: query => {
			return db
				.collection(query.collection)
				.doc(query.doc)
				.collection(query.subcollection)
				.doc(query.subdoc)
				.get()
				.then(doc => {
					if(doc.exists) {
						return doc.data();
					} else {
						return null;
					}
				})
				.catch(error => {
					console.log("Error getting document: ", error);
				});

		},
		saveToDbCollection: data => {
			return db.collection(data.collection)
				.doc(data.doc)
				.set(data.data, { merge: true })
				.then( function() {
					return;
				})
				.catch(error => {
					console.log('Error writing document', error);
				});
		},

		saveToDbSubcollection: data => {
			return db.collection(data.collection)
				.doc(data.doc)
				.collection(data.subcollection)
				.doc(data.subdoc)
				.set(data.data, { merge: true })
				.then( function() {
					return;
				})
				.catch(error => {
					console.log('Error writing document', error);
				});
		}
	};

}());
