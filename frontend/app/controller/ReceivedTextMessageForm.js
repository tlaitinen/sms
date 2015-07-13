Ext.define('SMS.controller.ReceivedTextMessageForm', {
    extend: 'Ext.app.Controller',
    createReplyMessage: function(button, success) {
        var form = button.up('form'),
            record = form.getRecord()
        button.disable();    
        Ext.Ajax.request({
            url: 'backend/db/textmessages/' + record.getId() + '/reply',
            jsonData: {},
            success: function(request) {
                button.enable();
                var r = JSON.parse(request.responseText);
                success(r);
            },
            failure: function(request) {
                button.enable();
                Ext.Msg.alert(__('saveError.title'), __('saveError.message') + ":" + request.responseText);
            }
        });

    },
    init: function() {
        var c = this;
        
        this.control({
           'receivedtextmessageform button[name=reply]' :{
               click: function(button) {
                    var formName = 'textmessageform';
                    var w = Ext.getCmp(formName + 'reply');
                    if (w)
                        w.close();

                    function success(record) {
                        Ext.getStore('textmessages').reload();
                        var win = new Ext.Window({
                            id: formName + 'reply',
                            title: __(formName + '.title'),
                            width:610,
                            height:500,
                            resizable:false,
                            items : [{xtype: formName}]
                        });
                        win.down('form').loadRecord(record);
                        win.show();
                    }
                    c.createReplyMessage(button, success);
               }
           }
        });
    }

});
