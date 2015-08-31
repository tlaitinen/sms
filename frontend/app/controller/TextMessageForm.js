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

    html2text: function(html) {
        var tag = document.createElement('div');
        tag.innerHTML = html;
        return tag.innerText;
    }, 
    spellCheckHtmlEditor: function(htmlEditor) {
        var controller = this;
        $.ajax({
            url:'backend/spell-check',
            dataType:'json',
            data: {
                text: controller.html2text(htmlEditor.getValue())
            }
        }).done(function (data) {
            var v = controller.html2text(htmlEditor.getValue());
            var words = v.split(/\s+/);
            for (var i = 0; i < words.length; i++) {
                var w = words[i];
                var tmp = w.replace(/[\u2000-\u206F\u2E00-\u2E7F\\'!"#$%&()*+,\-.\/:;<=>?@\[\]^_`{|}~]/, ''); 
                if (_.contains(data.unknownWords, tmp)) {
                    words[i] = w.replace(tmp, '<span style="background-color: #ffaaaa">' + tmp + '</span>');
                }
            }
            htmlEditor.setValue(words.join(" "));
            htmlEditor.up('form').down('textfield').focus();

        });
    },
    init: function() {
        var c = this;
        
        spellCheck = _.debounce(function(htmlEditor) { c.spellCheckHtmlEditor(htmlEditor); }, 3000);
        this.control({
           'textmessageform [name=text]' : {
                change: function (htmlEditor) {
                    var form = htmlEditor.up('form'),
                        record = form.getRecord();

                    var count = SmsCounter.count(c.html2text(htmlEditor.getValue()));
                    spellCheck(htmlEditor);


                    form.down('textfield[name=length]').setValue(__('textmessageform.lengthMessage').replace("{0}", ''+count.length).replace("{1}", ''+count.messages).replace("{2}", ''+count.per_message).replace("{3}", ''+count.remaining));

                }
           },
           'textmessageform' : {
               render: function(form) {
                   var record = form.getRecord();
                   if (record.get('replyToTextMessageId')) {
                       form.down('[name=replyToText]').show();
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
