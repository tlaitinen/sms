Ext.define('Receipts.view.main.MainController', {
    extend: 'Ext.app.ViewController',

    requires: [
        'Ext.window.MessageBox'
    ],

    alias: 'controller.main',

    onLogin: function() {
        if (Receipts.GlobalState.user.config.usersTab == true) {
            this.lookupReference('usersTab').tab.show();
        }
    },

    init: function() {
        var controller = this;
        Receipts.GlobalState.on('login', function() {
            controller.onLogin();
        });
    }

});
