Ext.define('Receipts.view.main.MainController', {
    extend: 'Ext.app.ViewController',

    requires: [
        'Ext.window.MessageBox'
    ],

    alias: 'controller.main',

    getProcessPeriodCombo: function() {
        return Ext.ComponentQuery.query('panel[name=receipts] receiptsgrid processperiodscombo')[0];
    },
    resetProcessPeriodCombo: function() {
        var ppCombo = this.getProcessPeriodCombo(),
            pps = Ext.getStore('processperiods');
        pps.load(function() {

            ppCombo.clearValue();
            if (pps.getCount() > 0) {
                ppCombo.setValue(pps.getAt(0).getId());
            }
            ppCombo.fireEvent('select', ppCombo);
            ppCombo.configStore();
        });
    },
    onLogin: function() {

        if (Receipts.GlobalState.user.config.usersTab == true) {
            this.lookupReference('usersTab').tab.show();
        }

        this.resetProcessPeriodCombo();
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
            'receiptsgrid' : {
                cellclick: function( grid, td, cellIndex, record, tr, rowIndex, e, eOpts ) {
                    if (cellIndex == 2) {
                        var win = new Ext.Window({
                            id:'preview',
                            layout:'fit',
                            width:'100%',
                            height:'100%',
                            closable:false,
                            resizable:false,
                            plain:true,
                            title: __('preview'),
                            items: [
                                { 
                                    xtype:'panel',
                                    html: '<img src="backend/file/' + record.get('previewFileId') + '"/>',
                                    autoScroll:true,
                                    listeners: {
                                       'render': function(panel) {
                                           panel.body.on('click', function() {
                                                win.close();
                                           });
                                        }
                                    }
                                }
                            ]
                        });
                        win.show();
                    }
                }

            },
            'receiptsgrid button[name=lock]': {
                click: function(button) {
                    Ext.MessageBox.confirm(__('confirmlock.title'), __('confirmlock.message'),
                        function (button) {
                        if (button == "yes") {
                            var ppCombo = controller.getProcessPeriodCombo();
                            Ext.Ajax.request({
                                url: 'backend/db/processperiods/' + ppCombo.getValue(),
                                method: 'POST',
                                params: {},
                                success: function () {
                                    controller.resetProcessPeriodCombo();
                                }
                            });
                        }
                    });
                }
            },
            'panel[name=users] button[name=addReadPerm]': {
                click: function(button) {
                    var panel = button.up('panel[name=users]');
                    controller.addUserGroupItems(panel.down('usersgrid'),
                                                 panel.down('usergroupsgrid'),
                                                 panel.down('usergroupitemsgrid'),
                                                 'ReadOnly');

                }
            },
            'panel[name=users] button[name=addWritePerm]': {
                click: function(button) {
                    var panel = button.up('panel[name=users]');
                    controller.addUserGroupItems(panel.down('usersgrid'),
                                                 panel.down('usergroupsgrid'),
                                                 panel.down('usergroupitemsgrid'),
                                                 'ReadWrite');
                }
            },
            'button[name=logout]' : {
                click: function(button) {
                    $.ajax({
                        url:'backend/auth/logout',
                        type:'POST',
                        dataType:'json'
                    }).always(function() {
                        location.reload();
                    });
                }
            }
        });
    }

});
