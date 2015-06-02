Ext.define('Receipts.controller.Login', {
    extend: 'Ext.app.Controller',
    views: ['Login'],
    init: function() {
        var win = new Ext.Window({
            id:'login',
            layout:'fit',
            width:'100%',
            height:'100%',
            closable:false,
            resizable:false,
            plain:true,
            title: __('login.title'),
            items: [{xtype:'login'}]
        });
        win.show();
        Ext.Ajax.request({
            url: 'backend/',
            success: function(response){

                try {
                    var obj = JSON.parse(response.responseText)
                    if ("user" in obj) {
                        win.close();
                        Receipts.GlobalState.user = obj.user;
                        try {
                            Receipts.GlobalState.user.config = JSON.parse(Receipts.GlobalState.user.config);
                        } catch (e) {
                            console.log("Warning: invalid user config: " + e);
                        }
                        Receipts.GlobalState.fireEvent('login');
                    }
                } catch (e) {}
            }
        });
        this.control({
            'login button[name=login]' : {
                click: function(button) {
                    var win = button.up('window'),
                        form = win.down('form');
                    form.submit({
                        method:'POST',
                        waitTitle:__('login.waittitle'),
                        waitMsg:__('login.waitmessage'),
                        headers: {
                            'Accept': 'application/json'
                        },
                        success:function(form, action) {
                            win.close();
                            Receipts.GlobalState.fireEvent('login');
                        },
                        failure:function(form, action) {
                            Ext.Msg.alert(__('login.failedtitle'), __('login.failedmessage'));
                        }
                    });
                }
            }
        });
    }

});
