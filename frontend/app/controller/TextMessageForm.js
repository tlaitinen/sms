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
                    var form = textarea.up('form'),
                        record = form.getRecord();

                    var count = SmsCounter.count(textarea.getValue());



                    form.down('textfield[name=length]').setValue(__('textmessageform.lengthMessage').replace("{0}", ''+count.length).replace("{1}", ''+count.messages).replace("{2}", ''+count.per_message).replace("{3}", ''+count.remaining));

                }
           },
           'textmessageform' : {
               render: function(form) {
                   var record = form.getRecord();
                   if (record.get('replyToTextMessageId')) {
                       form.down('textareafield[name=replyToText]').show();
                       form.up('window').setHeight(570);
                   }
                   if (record.get('queued') == null) {
                       form.down('button[name=send]').enable();
                   } else {

                       form.down('button[name=save]').disable();
                       form.down('button[name=saveandclose]').disable();
                       if (record.get('aborted') == null && record.get('sent') == null) {
                           form.down('button[name=abort]').enable();
                       }

                   }
               }
           },
           'textmessageform button[name=send]' :{
               click: function(button) {
                   var form = button.up('form'),
                       record = form.getRecord();
                   form.updateRecord(record);
                   record.save({
                       success: function() {
                           c.textMessageAction(button, 'queue');
                       }
                   });
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
