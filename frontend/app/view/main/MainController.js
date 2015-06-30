Ext.define('Receipts.view.main.MainController', {
    extend: 'Ext.app.ViewController',

    requires: [
        'Ext.window.MessageBox'
    ],

    alias: 'controller.main',
    config: {
        control: {
            '#maintab' : {
                tabchange: 'onTabChange'
            }
        },
        routes : {
            'maintab:id' : {
                action     : 'showTab',
                conditions : {
                    ':id'    : '(?:(?::){1}([%a-zA-Z0-9\-\_\s,]+))?'
                }
            },
            'preview:id' : {
                action     : 'showPreview',
                conditions : {
                    ':id'    : '(?:(?::){1}([%a-zA-Z0-9\-\_\s,]+))?'
                }
            }
        }
    },
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
        this.redirectTo('maintab:maintab-receipts');
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
    onTabChange: function(tabPanel, newItem) {
        var id = newItem.getId();
        this.redirectTo('maintab:' + id);
    },
    showTab : function(id) {    
        Ext.WindowManager.each(function(cmp) { cmp.destroy(); });
        var tabPanel = this.lookupReference('mainTab');
        if (!id) {
            id = 0;
        }
        var child = tabPanel.getComponent(id);
        tabPanel.setActiveTab(child);
        var previewWin = Ext.getCmp('preview');
        if (previewWin)
            previewWin.close();
    },
    showPreview: function(id) {
        var controller = this;
        var win = new Ext.Window({
            id:'preview',
            layout:'fit',
            width:'80%',
            height:'80%',
            closable:true,
            resizable:true,
            plain:true,
            title: __('preview'),
            items: [
                { 
                    xtype:'panel',
                    html: '<img src="backend/file/' + id + '"/>',
                    autoScroll:true,
                    listeners: {
                       'render': function(panel) {
                           panel.body.on('click', function() {
                                win.close();
                                controller.redirectTo('maintab:maintab-receipts');
                           });
                        }
                    }
                }
            ]
        });
        win.show();

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
                        controller.redirectTo('maintab:maintab-receipts|preview:' + record.get('previewFileId'));
                        controller.showPreview(record.get('previewFileId'));
                    }
                }

            },
            'receiptsgrid button[name=send]': {
                click: function(button) {
                    Ext.MessageBox.confirm(__('confirmsend.title'), __('confirmsend.message'),
                        function (button) {
                        if (button == "yes") {
                            var ppCombo = controller.getProcessPeriodCombo();
                            Ext.Ajax.request({
                                url: 'backend/db/processperiods/' + ppCombo.getValue(),
                                method: 'POST',
                                params: {
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
