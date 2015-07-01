/**
 * This class is the main view for the application. It is specified in app.js as the
 * "autoCreateViewport" property. That setting automatically applies the "viewport"
 * plugin to promote that instance of this class to the body element.
 *
 * TODO - Replace this content of this view to suite the needs of your application.
 */
Ext.define('SMS.view.main.Main', {
    extend: 'Ext.container.Container',
    requires: [
        'SMS.view.main.MainController'
    ],

    xtype: 'app-main',
    
    controller: 'main',

    layout: 'fit',

    items: [{
        region: 'center',
        xtype: 'tabpanel',
        id: 'maintab',
        reference: 'mainTab',
        tabBar: {
            items: [
                { xtype: 'tbfill' },
                {
                    text:__('maintab.signout'),
                    xtype:'button',
                    closable:false,
                    name:'logout'
                }
            ]
        },
        items:[
            {
                name:'clients',
                id:'maintab-clients',
                reference: 'clientsTab',
                title: __('maintab.clients'),
                layout:{
                    type:'vbox',
                    align:'stretch'
                },
                items: [
                    { xtype: 'clientsgrid', flex:3 }
                ]
            },
            {
                reference: 'usersTab',
                hidden:true,
                id:'maintab-users',
                title: __('maintab.users'),
                layout:'fit',
                items: [
                    {
                        xtype: 'panel',
                        name: 'users',
                        layout: {
                            type: 'vbox',
                            align:'stretch'
                        },
                        items: [
                            { xtype: 'usersgrid', flex:1,  autoscroll:true },
                            { xtype: 'usergroupsgrid', flex:1,  autoscroll:true },
                            {
                                xtype:'panel',
                                layout: {
                                    type:'hbox',
                                    align:'stretch'
                                },
                                
                                items: [
                                    { 
                                        xtype:'button', 
                                        name:'addReadPerm',
                                        text: __('users.addReadPerm')
                                    },
                                    {
                                        xtype:'button',
                                        name:'addWritePerm',
                                        text: __('users.addWritePerm')
                                    }
                                ]
                                 
                            },
                            { xtype: 'usergroupitemsgrid', flex:1,  autoscroll:true }
                        ]
                    }
                ]
            }
        ]
    }]
});
