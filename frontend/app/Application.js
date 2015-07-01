
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
        'Login@SMS.controller'
    ],

    stores: [
    ],
    
    init:function() {
        function boolRenderer(value,meta, record) {
            if (value == true)
                return '<span class="glyphicon glyphicon-ok"></span>';
            return ' ';
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
            $.get("resources/backend.json").done(function(defs) {
                console.log(Ext.util.Format.dateFormat);
                var config = {
                    name: 'SMS',
                    urlBase: 'backend/db',
                    defaultStoreFilters: [
                        {
                            field:'hideDeleted',
                            value:"true"
                        }
                    ],
                    routes: {
                        clients: {

                            autoSync:true,
                            grids: [
                                {
                                    widget: 'clientsgrid',
                                    globalStore:true,
                                    preload:false,
                                    plugins: 'rowediting',
                                    columns: [ 
                                        { field:'firstName', editor: { allowBlank:false}, flex:3 }, 
                                        { field:'lastName', editor: { allowBlank:false}, flex:5 }, 
                                        { field:'email', editor: { }, flex:5 },
                                        { field:'phone', editor: {}, flex:3 },
                                        { 
                                            field: 'dateOfBirth', 
                                            editor:{ xtype:'datefield'}, 
                                            flex:2,
                                            xtype:'datecolumn',
                                            format:Ext.util.Format.dateFormat 
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
                                        { name: 'remove', action:'remove' }
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
               var ydsl = yesodDsl(defs, __, config);
           });
       });
   }
});
Ext.define('SMS.GlobalState', {
    extend: 'Ext.util.Observable',
    singleton:true
});



