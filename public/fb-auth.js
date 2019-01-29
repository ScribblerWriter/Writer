const fbauth = (function(messageCallback) {
	// Initialize authentication
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
