Ext.define('Receipts.view.main.MainController', {
    extend: 'Ext.app.ViewController',

    requires: [
        'Ext.window.MessageBox'
    ],

    alias: 'controller.main',

    onLogin: function() {
        if (Receipts.GlobalState.user.config.usersTab == true) {
            this.lookupReference('usersTab').tab.show();
        }
    },

    addUserGroupItems: function(userGrid, userGroupGrid, userGroupItemsGrid, mode) {
        var users = userGrid.getSelectionModel().getSelection(),
            userGroups = userGroupGrid.getSelectionModel().getSelection(),
            userGroupItems = userGroupItemsGrid.store;
        for (var i = 0; i < users.length; i++) {
            for (var i2 = 0; i2 < userGroups.length; i2++) {
                (function(user, userGroup) {
                    userGroupItems.add(Ext.create('Receipts.model.usergroupitems',
                            {
                            userId: user.getId(),
                            userName: user.getData()['name'],
                           userGroupId : userGroup.getId(),
                            userGroupName: userGroup.getData()['name'],
                          mode: mode }));
                 })(users[i], userGroups[i2]);
            }
        }
        userGroupItems.sync();
    },
    init: function() {
        var controller = this;
        Receipts.GlobalState.on('login', function() {
            controller.onLogin();
            
        });

        this.control({
            'panel[name=users] button[name=addReadPerm]': {
                click: function(button) {
                    var panel = button.up('panel');
                    controller.addUserGroupItems(panel.down('usersgrid'),
                                                 panel.down('usergroupsgrid'),
                                                 panel.down('usergroupitemsgrid'),
                                                 'ReadOnly');

                }
            },
            'panel[name=users] button[name=addWritePerm]': {
                click: function(button) {
                    var panel = button.up('panel');
                    controller.addUserGroupItems(panel.down('usersgrid'),
                                                 panel.down('usergroupsgrid'),
                                                 panel.down('usergroupitemsgrid'),
                                                 'ReadWrite');
                }
            }
        });
    }

});
