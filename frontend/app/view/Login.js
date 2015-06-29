Ext.define('Receipts.view.Login',{
    extend: 'Ext.form.Panel',
    alias: 'widget.login',
    layout: {
        type: 'vbox',
        align:'center',
        pack:'center'
    },
    url:'backend/auth/page/hashdb/login',
    errorReader: 'customreader',
    items: [
        { 
            xtype: 'form',
            defaultType:'textfield',
            title: __('login.title'),
            bodyPadding:10,
            items: [
                {
                    fieldLabel: __('login.username'),
                    name: 'username',
                    enableKeyEvents:true,
                    allowBlank:false
                },
                {
                    fieldLabel : __('login.password'),
                    name : 'password',
                    inputType:'password',
                    enableKeyEvents:true,
                    allowBlank:false
                }
            ],
            buttons:[{ 
                text:__('login.login'),
                name:'login'
            }]
        }
    ]
});
