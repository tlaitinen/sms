Ext.define('SMS.controller.TextMessageForm', {
    extend: 'Ext.app.Controller',
    textMessageAction: function(button, action) {
        var form = button.up('form'),
            record = form.getRecord()
        button.disable();    
        Ext.Ajax.request({
            url: 'backend/db/textmessages/' + record.getId() + '/' + action,
            jsonData: {},
            success: function(request) {
                if (action == "queue") {
                    form.down('button[name=abort]').enable();
                    record.set('queued', new Date());
                } else if (action == "abort") {
                    record.set('aborted', new Date());
                }

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
           'textmessageform textareafield[name=text]' : {
                change: function (textarea) {
                    textarea.up('form').down('panel[name=length]').setHtml("Viestin pituus: " + textarea.getValue().length);

                }
           },
           'textmessageform' : {
               render: function(form) {
                   var record = form.getRecord();
                   if (record.get('phone') == '') {
                       if (record.get('queued') == null) {
                           form.down('button[name=send]').enable();
                       } else {
                           form.down('button[name=save]').disable();
                           form.down('button[name=saveandclose]').disable();
                           if (record.get('aborted') == null) {
                               form.down('button[name=abort]').enable();
                           }
                       }
                   } else {
                       form.down('button[name=save]').hide();
                       form.down('button[name=saveandclose]').hide();
                       form.down('button[name=send]').hide();
                       form.down('button[name=abort]').hide();
                       form.down('grid').hide();
                       form.down('textareafield[name=text]').setReadOnly(true);
                   }
               }
           },
           'textmessageform button[name=send]' :{
               click: function(button) {
                   c.textMessageAction(button, 'queue');
               }
           },
           'textmessageform button[name=abort]' : {
               click: function(button) {
                   c.textMessageAction(button, 'abort');
               }
           }
        });
    }

});
