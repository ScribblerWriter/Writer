const fbauth = (function(messageCallback) {
	const config = {
	  apiKey: "AIzaSyA7yISzO1I4rss7s1mykVmBjMAKGDExRjo",
	  authDomain: "writer-788cf.firebaseapp.com",
	  databaseURL: "https://writer-788cf.firebaseio.com",
	  projectId: "writer-788cf",
	  storageBucket: "writer-788cf.appspot.com",
	  messagingSenderId: "181941734046"
	};
	  
	firebase.initializeApp(config);

	const auth = firebase.auth();

	auth.onAuthStateChanged(user => {
		if (user) {
			console.log(user);
		} else {
			console.log('Not logged in.');
		}

		messageCallback({
			operation : "AuthStateChanged",
			content : user
		});
	});

	return {
		login: data => {
			auth.signInWithEmailAndPassword(data.email, data.pass)
				.catch(error => {
					console.log('Error logging in: ', error.message)
				});
		},

		signup: data => {
			auth.createUserWithEmailAndPassword(data.email, data.pass)
				.catch(error => {
					console.log('Error creating new user: ', error.message)
				});
		},

		signout: () => {
			auth.signOut()
				.catch(error => {
					console.log('Error signing out: ', error.message)
				});
		}
	};

}(messageCallback));