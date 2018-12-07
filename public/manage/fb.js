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
	  saveToDB: function(collection, id, data) {
	    db.collection(collection).doc(id).set(data)
	    .then(function() {
	      console.log("Document written");
	    })
	    .catch(function(error) {
	      console.error("Error adding document: ", error);
	    });
	  }
	};

}());




// const fbdb = (function() {

// 	// initialize Firebase
// 	const config = {
// 		apiKey: "AIzaSyA7yISzO1I4rss7s1mykVmBjMAKGDExRjo",
// 		authDomain: "writer-788cf.firebaseapp.com",
// 		databaseURL: "https://writer-788cf.firebaseio.com",
// 		projectId: "writer-788cf",
// 		storageBucket: "writer-788cf.appspot.com",
// 		messagingSenderId: "181941734046"
// 	};
    
// 	firebase.initializeApp(config);

// 	// Initialize database
// 	const db = firebase.firestore();

// 	// Disable deprecated features
// 	db.settings({
// 		timestampsInSnapshots: true
// 	});

// 	return {
// 		saveToDb: function(collection, data) {
// 			db.collection(collection).set(data)
// 			.then(function(docRef) {
// 				console.log("Document ID: ", docRef.id);
// 			})
// 			.catch(function(error) {
// 				console.error("Error adding document: ", error);
// 			});
// 		}
// 	};
// }());