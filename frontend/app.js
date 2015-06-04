
$.get("backend.json").done(function(defs) {
    var config = {
        name: 'Receipts',
        urlBase: 'backend/db',
        defaultStoreFilters: [
            {
                field:'hideDeleted',
                value:"true"
            }
        ],
        routes: {
            receipts: {

                autoSync:true,
                grids: [
                    {
                        widget: 'receiptsgrid',
                        globalStore:true,
                        plugins: 'cellediting',
                        columns: [ 
                            { field:'name', editor: { allowBlank:false}, flex:5 }, 
                            { field:'amount', editor: {}, flex:1 },
                            { field:'previewFileId', flex:1 } 
                        ],
                        bottomToolbar: [
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
                        ]
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
                        items: [ 'name' ]
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

    Ext.application({
        name: 'Receipts',

        extend: 'Receipts.Application',
        

        ydsl: ydsl
        
        //-------------------------------------------------------------------------
        // Most customizations should be made to Receipts.Application. If you need to
        // customize this file, doing so below this section reduces the likelihood
        // of merge conflicts when upgrading to new versions of Sencha Cmd.
        //-------------------------------------------------------------------------
    });

});
