const displayMessage = (function() {
    return {
        build: (body, severity, source) => {
            return { operation: "DisplayMessageReceived", content: { body: body, severity: severity, source: source}};
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
