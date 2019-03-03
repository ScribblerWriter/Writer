const fbauth = (function(messageCallback) {
	// Initialize authentication
	const auth = firebase.auth();

	auth.onAuthStateChanged(user => {
		messageCallback({
			operation : "AuthStateChanged",
			content : user
		});
	});

	function buildWarningMessage(error) {
		return displayMessage.build(
			error.message,
			displayMessage.severityWarning(),
			displayMessage.sourceAuth(),
			error.code
		);
	}
	return {
		signIn: data => {
			auth.signInWithEmailAndPassword(data.email, data.pass)
				.catch(error => {
					messageCallback(buildWarningMessage(error));
				});
		},

		signUp: data => {
			auth.createUserWithEmailAndPassword(data.email, data.pass)
				.catch(error => {
					messageCallback(buildWarningMessage(error));
				});
		},

		signOut: () => {
			auth.signOut()
				.catch(error => {
					messageCallback(buildWarningMessage(error));
				});
		}
	};

}(messageCallback));
