/**
 * The main application class. An instance of this class is created by app.js when it calls
 * Ext.application(). This is the ideal place to handle application launch and initialization
 * details.
 */
Ext.define('Receipts.Application', {
    extend: 'Ext.app.Application',
    
    name: 'Receipts',

    controllers: [
        'Login@Receipts.controller'
    ],

    stores: [
    ],
    
    launch: function () {
        var app = this;
        $.get("backend.json").done(function(defs) {
            var config = {
                name: 'Receipts',
                routes: {
                }
            };
            yesodDsl(defs, __, config);
         
        });
    }
});
Ext.define('Receipts.GlobalState', {
    extend: 'Ext.util.Observable',
    singleton:true
});
Ext.define('Receipts.CustomReader', {
    extend: 'Ext.data.reader.Reader',
    alias: 'reader.customreader',
    read: function(xhr)  {
        return {success:  xhr.status == 200 };
    }
});
