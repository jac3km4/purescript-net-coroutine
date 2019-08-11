'use strict';
const tls = require('tls');

exports._connect = function (port, host, options) {
    return function (onError, onSuccess) {
        const socket = tls.connect(port, host, options, function () {
            if (socket.authorized) onError(socket.authorizationError);
            else onSuccess(socket);
        });
        return function (cancelError, onCancelerError, onCancelerSuccess) {
            socket.end(function () {
                onCancelerSuccess();
            })
        };
    }
}
