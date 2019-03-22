const fbauth = (function(messageCallback) {
	// Initialize authentication
	const auth = firebase.auth();

	auth.onAuthStateChanged(user => {
		messageCallback({
			operation : "AuthStateChanged",
			content : user
		});
	});

	function authenticationMessage(error) {
		return { operation: "AuthMsgReceived"
			   , content: error.message
		};
	}

	return {
		signIn: data => {
			auth.signInWithEmailAndPassword(data.email, data.pass)
				.catch(error => {
					messageCallback(authenticationMessage(error));
				});
		},

		signUp: data => {
			auth.createUserWithEmailAndPassword(data.email, data.pass)
				.catch(error => {
					messageCallback(authenticationMessage(error));
				});
		},

		signOut: () => {
			auth.signOut()
				.catch(error => {
					messageCallback(authenticationMessage(error));
				});
		}
	};

}(messageCallback));
