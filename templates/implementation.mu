#import "{{ master.name }}.h"

@implementation {{ master.name }}

- (id)initWithFrame:(CGRect)frame
{
    if (!(self = [super initWithFrame:frame])) {
        return nil;
    }

{{#subviews}}
    UIView *{{subviewName}} = [[UIView alloc] initWithFrame:CGRectMake({{subviewFrame.x}}, {{subviewFrame.y}}, {{subviewFrame.w}}, {{subviewFrame.h}})];
    [{{subviewParent}} addSubview:{{subviewName}}];

{{/subviews}}

    [self setNeedsLayout];

    return self;
}

- (void)layoutSubviews
{
{{#layoutUpdaters}}
    // OHAI
{{/layoutUpdaters}}
}

@end

