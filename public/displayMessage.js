const displayMessage = (function() {
    function fbCodeToInternal (code) {
        console.log('error code:', code);
        if (code == 'auth/wrong-password') {
            return 'bad-pass';
        } else if (code == 'auth/user-not-found') {
            return 'bad-user';
        } else if (code == 'auth/invalid-email') {
            return 'invalid-email';
        } else if (code == 'auth/weak-password') {
            return 'weak-password';
        } else if (code == 'auth/email-already-in-use') {
            return 'email-already-in-use';
        } else {
            return "unknown";
        }
    }

    return {
        build: (body, severity, source, code) => {
            const internalCode = fbCodeToInternal(code);
            return { operation: "DisplayMessageReceived",
                     content: { body: body,
                                severity: severity,
                                source: source,
                                code: internalCode
                              }
                   };
        },

        sourceDb: () => {
            return "db";
        },

        sourceAuth: () => {
            return "auth";
        },

        sourceExternal: () => {
            return "external";
        },

        severityInfo: () => {
            return "info";
        },

        severityWarning: () => {
            return "warning";
        },

        severityError: () => {
            return "error";
        }
    }
}());
