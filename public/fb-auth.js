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
		signIn: data => {
			auth.signInWithEmailAndPassword(data.email, data.pass)
				.catch(error => {
					console.log('Error signing in: ', error.message)
				});
		},

		signUp: data => {
			auth.createUserWithEmailAndPassword(data.email, data.pass)
				.catch(error => {
					console.log('Error creating new user: ', error.message)
				});
		},

		signOut: () => {
			auth.signOut()
				.catch(error => {
					console.log('Error signing out: ', error.message)
				});
		}
	};

}(messageCallback));
