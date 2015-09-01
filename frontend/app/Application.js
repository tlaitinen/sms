
/**
 * The main application class. An instance of this class is created by app.js when it calls
 * Ext.application(). This is the ideal place to handle application launch and initialization
 * details.
 */
Ext.define('SMS.Application', {
    extend: 'Ext.app.Application',
    
    name: 'SMS',
    requires: [
        'Ext.data.proxy.Rest', 
        'Ext.form.field.Checkbox',
        'Ext.grid.Panel',
        'Ext.grid.plugin.CellEditing',
        'Ext.grid.plugin.RowEditing'
    ],
    controllers: [
        'Login@SMS.controller',
        'TextMessageForm@SMS.controller',
        'ReceivedTextMessageForm@SMS.controller'
    ],

    stores: [
    ],
    
    init:function() {
        function senderRenderer(value, meta, record) {
            if (record.get('firstName')) {
                return record.get('firstName') + " " + record.get('lastName');
            } else 
                return record.get('phone');
        }
        function boolRenderer(value,meta, record) {
            if (value == true)
                return '<span class="glyphicon glyphicon-ok"></span>';
            return ' ';
        }
        function notNullRenderer(value ,meta, record) {
            return boolRenderer(value != null, meta, record);
        }
        function dateRenderer(value,meta,record) {
            return Ext.Date.format(value, Ext.util.Format.dateFormat);
        }
        function dateTimeRenderer(value,meta,record) {
            return Ext.Date.format(value, Ext.util.Format.dateFormat + " H:i:s");
        }
     

        Ext.History.init();
        Ext.define('SMS.CustomReader', {
            extend: 'Ext.data.reader.Reader',
            alias: 'reader.customreader',
            read: function(xhr)  {
                return {success:  xhr.status == 200 };
            }
        }, function (customReader) {
            function maybeCreateDefaultUserGroup(form, record) {
                if (!record.get('defaultUserGroupId')) {
                    var ug = Ext.create('SMS.model.UserGroup');
                    ug.set('name', record.get('name'));
                    ug.set('email', SMS.GlobalState.user.defaultUserGroupEmail);
                    var dfd = jQuery.Deferred();
                    ug.save({
                        success: function (rec, op) {
                            try {
                                var r = JSON.parse(op.getResponse().responseText);
                                record.set('defaultUserGroupId', r.id);
                                dfd.resolve();;
                            } catch (e) {
                                console.log(e);
                            }
                        }
                    });
                    return dfd;
                } else {
                    return true;
                }
            }
            function stripTags(x) {
                return x.replace(/(<([^>]+)>)/ig,"");
            }
            $.get("resources/backend.json").done(function(defs) {
                var htmlEditorExtra = {
                    enableAlignments : false,
                    enableColors : false,
                    enableFont : false,
                    enableFontSize : false,
                    enableFormat : false,
                    enableLinks : false,
                    enableLists : false,
                    enableSourceEdit : false,
                    listeners : {
                        render: function(htmleditor) {
                            htmleditor.getToolbar().hide();
                        }
                    }
                };
                var config = {
                    name: 'SMS',
                    urlBase: 'backend/db',
                    defaultStoreFilters: [
                        {
                            field:'deletedVersionId',
                            value:null,
                            comparison:'is'
                        }
                    ],
                    routes: {
                        textmessages: {
                            grids: [
                                {
                                    widget: 'textmessagesgrid',
                                    globalStore:true,
                                    columns: [
                                        {
                                            field: 'insertionTime',
                                            flex:1,
                                            renderer:dateTimeRenderer
                                        },
                                        {
                                            field:'text',
                                            flex:5
                                        },
                                        {
                                            header:'sender',
                                            renderer:senderRenderer
                                        },
                                        {
                                            flex:1,
                                            field:'sent',
                                            renderer:dateTimeRenderer
                                        }
                                    ],
                                    bottomToolbar: [
                                        { name:'new', action:'new'},
                                        { name:'remove', action:'remove'}
                                    ],
                                    form: function(record) { 
                                        if (record.get('phone'))
                                            return 'receivedtextmessageform';
                                        else
                                            return 'textmessageform';
                                    },
                                    formHeight: function(record) {
                                        if (record.get('phone'))
                                            return 260;
                                        else
                                            return 500;
                                    }
                                }
    
                            ],
                            forms: [
                                {
                                    widget: 'textmessageform',
                                    filters: [ 'textMessageId' ],
                                    items: [
                                        {
                                            xtype:'htmleditor',
                                            name:'replyToText',
                                            hidden:true,
                                            readOnly:true,
                                            extra: htmlEditorExtra
                                        },
                                        {
                                            xtype:'htmleditor',
                                            name:'text',
                                            extra: htmlEditorExtra
                                        },
                                        {
                                            xtype:'textfield',
                                            name:'length',
                                            readOnly:true
                                        },
                                        {
                                            xtype:'textmessagerecipientsgrid',
                                            height:300
                                        },
                                        {
                                            xtype:'panel',
                                            layout: {
                                                type:'hbox',
                                                align:'stretch'
                                            },
                                            items: [
                                                {
                                                    xtype:'button',
                                                    name:'send',
                                                    flex:1,
                                                    disabled:true
                                                },
                                                {
                                                    xtype:'button',
                                                    name:'abort',
                                                    disabled:true,
                                                    flex:1
                                                }
                                            ]
                                        }
                                    ]
                                },
                                {
                                    widget: 'receivedtextmessageform',
                                    buttons: ['close'],
                                    items: [
                                        {
                                            xtype:'textareafield',
                                            name:'text',
                                            readOnly:true
                                        },
                                        {
                                            xtype:'textfield',
                                            name:'firstName',
                                            readOnly:true
                                        },
                                        {
                                            xtype:'textfield',
                                            name:'lastName',
                                            readOnly:true
                                        },
                                        {
                                            xtype:'textfield',
                                            name:'phone',
                                            readOnly:true
                                        },
                                        {
                                            xtype:'button',
                                            name:'reply'
                                        }
                                    ]
                                }
                            ]
                        },

                        clients: {
                            autoSync:true,

                            grids: [
                                {
                                    widget: 'clientsgrid',
                                    globalStore:true,
                                    preload:true,
                                    plugins: {
                                        pluginId: 'rowediting',
                                        ptype: 'rowediting',
                                        saveBtnText: __('rowedit.save'),
                                        cancelBtnText: __('rowedit.cancel')
                                    },
                                    toolbar: [
                                        {
                                            xtype:'monthcombo',
                                            filterField: 'dateOfBirthMonth'
                                        }
                                    ],
                                    columns: [ 
                                        { field:'firstName', editor: { allowBlank:false}, flex:3 }, 
                                        { field:'lastName', editor: { allowBlank:false}, flex:5 }, 
                                        { field:'email', editor: { }, flex:3 },
                                        { field:'phone', editor: {}, flex:3 },
                                        { 
                                            field: 'dateOfBirth', 
                                            editor:{ 
                                                xtype:'datefield',
                                                submitFormat: 'Y-m-d'
                                            }, 
                                            flex:2,
                                            renderer:dateRenderer
                                        },
                                        { field:'card', editor:{}, flex:3 },
                                        { 
                                            field:'allowSms', editor:{ xtype: 'checkbox'}, flex:1,
                                            renderer:boolRenderer
                                        },
                                        { field:'allowEmail', editor:{ xtype:'checkbox'}, flex:1,
                                            renderer:boolRenderer
                                        }
                                   ],
                                    bottomToolbar: [
                                        { name: 'add', action:'add' },
                                        { name: 'remove', action:'remove' },
                                        { name: 'sendMessage' }
                                    ]
                                }
                            ]
                        },
                        textmessagerecipients: {
                            grids: [
                                {
                                    widget:'textmessagerecipientsgrid',
                                    columns: [ 'firstName', 'lastName', 'phone',
                                        {
                                            field:'sent',
                                            renderer:notNullRenderer
                                        },
                                        {
                                            field:'delivered',
                                            renderer:notNullRenderer
                                        }
                                    ]
                                }
                            ]
                        },
                        users: {
                            grids : [
                                {
                                    widget: 'usersgrid',
                                    columns: [ 'name', 'firstName', 'lastName' ],
                                    toolbar: [
                                        {
                                            xtype: 'usergroupscombo',
                                            filterField: 'userGroupId'
                                        }
                                    ],
                                    bottomToolbar: [ 
                                        { name: 'new', action:'new' },
                                        { name: 'remove', action:'remove' } 
                                    ], 
                                    form:'userform',
                                    formHeight:300
                                }
                            ],
                            forms: [
                                {
                                    widget: 'userform',
                                    items: [
                                        'name', 'firstName', 'lastName', 'email', 
                                        'timeZone',
                                        'defaultUserGroupId',
                                        { xtype:'button', name:'setUserPassword', 
                                         form:'userpasswordform', formHeight:200 }
                                    ],
                                    beforeSubmit: [maybeCreateDefaultUserGroup],
                                    success: [function(record) {
                                        ['usergroupsgrid', 'usergroupitemsgrid'].forEach(function (n) {
                                            Ext.ComponentQuery.query(n).forEach(function(g) { g.store.reload(); });
                                        });
                                    }]
                                },
                                {
                                    widget: 'userpasswordform',
                                    items: [
                                        { 
                                            name: 'password', 
                                            xtype:'textfield', 
                                            inputType: 'password',
                                            minLength:6,
                                            minLengthText: 'passwordMinLength'
                                        },
                                        {
                                            name: 'passwordAgain',
                                            xtype:'textfield',
                                            inputType: 'password'
                                        }
                                    ],
                                    validation:function (values) {
                                        if (values.password != values.passwordAgain) {
                                            return 'validationError.password';
                                        }
                                    },
                                    buttons: [
                                        {
                                            name: 'ok',
                                            action: 'saveandclose',
                                            url: 'backend/set-user-password/(ID)',
                                            updateRecord:false
                                        },
                                        'closewithoutsaving'
                                    ]
                                }
                            ]
                        },
                        usergroups: {
                            grids: [
                                {
                                    widget: 'usergroupsgrid',
                                    globalStore:true,
                                    columns: [ 'name' ], 
                                    form:'usergroupform',
                                    bottomToolbar: [ 
                                        { name: 'new', action:'new' },
                                        { name: 'remove', action:'remove' } 
                                    ],
                                    formHeight:200
                                }
                            ],
                            forms: [
                                {
                                    widget: 'usergroupform',
                                    items: [ 'name', 'email', 'createPeriods' ]
                                }
                            ]
                        },
                        usergroupitems: {
                            grids: [
                                {
                                    widget: 'usergroupitemsgrid',
                                    globalStore:true,
                                    searchField:false,
                                    columns: [
                                        { field: 'userName', flex:2 },
                                        { field: 'userGroupName', flex:2 },
                                        { field: 'mode', flex:1 }
                                    ],
                                    bottomToolbar: [
                                        { name:'remove', action:'remove' }
                                    ]

                                }
                            ]
                        },
                        usergroupcontents: {
                            grids: [
                                {
                                    searchField:false,
                                    widget:'usergroupcontentsgrid',
                                    columns: [ 'userGroupName' ],
                                    bottomToolbar: [
                                        { name:'remove', action:'remove' }
                                    ]
                                }
                            ]
                        }
                    }
                };
               var onReady = function() {
                   console.log("onReady!");
                   SMS.GlobalState.fireEvent('ready');
               }
               window.yesodDsl = yesodDsl(defs, __, config, onReady);
           });
       });
   }
});
Ext.define('SMS.GlobalState', {
    extend: 'Ext.util.Observable',
    singleton:true
});



